use graphql_parser::query::parse_query;
use pathdiff::diff_paths;
use serde::Deserialize;
use std::path::{Path, PathBuf};
use swc_core::{
    common::Span,
    ecma::{
        ast::*,
        utils::quote_ident,
        visit::{visit_mut_pass, VisitMut, VisitMutWith},
    },
    plugin::{
        errors::HANDLER, metadata::TransformPluginMetadataContextKind, plugin_transform,
        proxies::TransformPluginProgramMetadata,
    },
};

fn capetalize(s: &str) -> String {
    format!("{}{}", (&s[..1].to_string()).to_uppercase(), &s[1..])
}

fn to_pascal_case(s: &str) -> String {
    // The basic approach:
    // 1. Split the input string into words based on case changes, underscores, etc.
    // 2. Capitalize each word
    // 3. Join the words back together

    let mut result = String::new();
    let mut current_word = String::new();

    // Track previous character type to detect word boundaries
    let mut prev_is_lowercase = false;
    let mut prev_is_uppercase = false;
    let mut prev_is_numeric = false;

    for (i, c) in s.chars().enumerate() {
        let is_delimiter = c == '_' || c == '-';
        let is_uppercase = c.is_uppercase();
        let is_lowercase = c.is_lowercase();
        let is_numeric = c.is_numeric();

        // Handle delimiters - finish the current word
        if is_delimiter {
            if !current_word.is_empty() {
                result.push_str(&capitalize_word(&current_word));
                current_word.clear();
            }
        }
        // Detect camelCase boundary (lowercase followed by uppercase)
        else if i > 0 && is_uppercase && prev_is_lowercase {
            result.push_str(&capitalize_word(&current_word));
            current_word.clear();
            current_word.push(c);
        }
        // Detect "acronym end" boundary (multiple uppercase followed by lowercase: HTTPRequest -> HTTP/Request)
        else if i > 0 && is_lowercase && prev_is_uppercase && i >= 2 {
            // Check if we have at least 2 uppercase chars in sequence
            let last_char = current_word.chars().last().unwrap();
            current_word.pop(); // Remove the last char

            if !current_word.is_empty() {
                result.push_str(&capitalize_word(&current_word));
                current_word.clear();
            }

            current_word.push(last_char);
            current_word.push(c);
        }
        // Normal character - add to current word
        else {
            current_word.push(c);
        }

        // Update previous character state
        prev_is_lowercase = is_lowercase;
        prev_is_uppercase = is_uppercase;
        prev_is_numeric = is_numeric;
    }

    // Add the final word if there is one
    if !current_word.is_empty() {
        result.push_str(&capitalize_word(&current_word));
    }

    result
}

fn capitalize_word(word: &str) -> String {
    let mut chars = word.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str().to_lowercase().as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests;

pub struct GraphQLCodegenOptions {
    pub filename: String,
    pub cwd: String,
    pub artifact_directory: String,
    pub gql_tag_name: String,
}

pub struct GraphQLVisitor {
    options: GraphQLCodegenOptions,
    graphql_operations_or_fragments_to_import: Vec<String>,
}

impl GraphQLVisitor {
    pub fn new(options: GraphQLCodegenOptions) -> Self {
        GraphQLVisitor {
            options,
            graphql_operations_or_fragments_to_import: Vec::new(),
        }
    }

    fn handle_error(&self, details: &str, span: Span) {
        let message = format!(
            "@graphql-codegen/client-preset-swc-plugin details: {}",
            details
        );
        HANDLER.with(|handler| handler.struct_span_err(span, &message).emit());
    }

    fn get_relative_import_path(&self, path_end: &str) -> String {
        // Build the full path to the current file
        let mut file_full_path = PathBuf::from(&self.options.cwd);
        file_full_path.push(&self.options.filename);
        let file_s_dirname = file_full_path.parent().unwrap();

        // Resolve the artifact directory
        let mut resolved_artifact_directory = if Path::new(&self.options.artifact_directory).is_relative() {
            let mut cwd = PathBuf::from(&self.options.cwd);
            cwd.push(&self.options.artifact_directory);
            cwd
        } else {
            PathBuf::from(&self.options.artifact_directory)
        };

        // Add the file (e.g. graphql.ts) to the artifact directory
        resolved_artifact_directory.push(path_end);

        // Compute the relative path from the file's directory to the artifact file
        let relative = diff_paths(&resolved_artifact_directory, file_s_dirname)
            .unwrap_or_else(|| panic!(
                "Could not compute relative path from {:?} to {:?}",
                file_s_dirname, resolved_artifact_directory
            ));

        // Convert to string and normalize separators
        relative.to_str().unwrap().replace("\\", "/")
    }
}

pub fn create_graphql_codegen_visitor(options: GraphQLCodegenOptions) -> impl VisitMut {
    GraphQLVisitor::new(options)
}

impl VisitMut for GraphQLVisitor {
    fn visit_mut_var_decl(&mut self, e: &mut VarDecl) {
        e.visit_mut_children_with(self);

        for decl in e.decls.iter_mut() {
            if let Some(init) = &mut decl.init {
                if let Expr::Call(CallExpr { callee, args, .. }) = &mut **init {
                    if args.is_empty() {
                        return;
                    }

                    match callee.as_expr() {
                        Some(expr_box) => match &**expr_box {
                            Expr::Ident(ident) => {
                                if &ident.sym != self.options.gql_tag_name.as_str() {
                                    return;
                                }
                            }
                            _ => return,
                        },
                        _ => return,
                    }

                    let quasis = match &*args[0].expr {
                        Expr::Tpl(tpl) => &tpl.quasis,
                        _ => return,
                    };

                    let raw = match &quasis[0].cooked {
                        Some(cooked) => cooked,
                        None => return,
                    };

                    let graphql_ast = match parse_query::<&str>(raw) {
                        Ok(ast) => ast,
                        Err(e) => {
                            // Currently the parser outputs a string like: "query parse error", so we add "GraphQL" to the beginning
                            let error = format!("GraphQL {}", e);
                            self.handle_error(error.as_str(), quasis[0].span);
                            return;
                        }
                    };

                    let first_definition = match graphql_ast.definitions.get(0) {
                        Some(definition) => definition,
                        None => return,
                    };

                    let operation_name = match first_definition {
                        graphql_parser::query::Definition::Fragment(fragment) => {
                            to_pascal_case(&fragment.name) + "FragmentDoc"
                        }
                        graphql_parser::query::Definition::Operation(op) => match op {
                            graphql_parser::query::OperationDefinition::Query(query) => {
                                match query.name {
                                    Some(name) => to_pascal_case(&name) + "Document",
                                    None => return,
                                }
                            }
                            graphql_parser::query::OperationDefinition::Mutation(mutation) => {
                                match mutation.name {
                                    Some(name) => to_pascal_case(&name) + "Document",
                                    None => return,
                                }
                            }
                            graphql_parser::query::OperationDefinition::Subscription(
                                subscription,
                            ) => match subscription.name {
                                Some(name) => to_pascal_case(&name) + "Document",
                                None => return,
                            },
                            _ => return,
                        },
                    };

                    self.graphql_operations_or_fragments_to_import
                        .push(operation_name.clone());

                    // now change the call expression to a Identifier
                    let new_expr = Expr::Ident(quote_ident!(operation_name.clone()).into());

                    *init = Box::new(new_expr);
                }
            }
        }
    }

    fn visit_mut_module(&mut self, module: &mut Module) {
        // First visit all its children, collect the GraphQL document names, and then add the necessary imports
        module.visit_mut_children_with(self);

        if self.graphql_operations_or_fragments_to_import.is_empty() {
            return;
        }

        let platform_specific_path = self.get_relative_import_path("graphql.ts");

        // Add import after any "use client" directive, since it must come before any other expression
        let mut index = 0;

        if let Some(ModuleItem::Stmt(Stmt::Expr(ExprStmt { expr, .. }))) = module.body.first() {
            if let Expr::Lit(Lit::Str(Str { value, .. })) = &**expr {
                if matches!(value.as_ref(), "use client" | "use server") {
                    index = 1; // Only one directive can exist, so we stop after the first match
                }
            }
        }

        for operation_or_fragment_name in &self.graphql_operations_or_fragments_to_import {
            module.body.insert(
                index,
                ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                    span: Default::default(),
                    specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
                        span: Default::default(),
                        local: quote_ident!(operation_or_fragment_name.to_string()).into(),
                        imported: None,
                        is_type_only: false,
                    })],
                    src: Box::new(Str::from(platform_specific_path.to_string())),
                    type_only: false,
                    with: None,
                    phase: ImportPhase::default()
                })),
            )
        }
    }
}

fn gql_default() -> String {
    "gql".to_string()
}
#[allow(non_snake_case)]
#[derive(Deserialize)]
struct PluginOptions {
    artifactDirectory: String,

    #[serde(default = "gql_default")]
    gqlTagName: String,
}

#[plugin_transform]
pub fn process_transform(program: Program, metadata: TransformPluginProgramMetadata) -> Program {
    let filename = metadata
        .get_context(&TransformPluginMetadataContextKind::Filename)
        .unwrap_or_default();
    let cwd = metadata
        .get_context(&TransformPluginMetadataContextKind::Cwd)
        .unwrap_or_default();

    let plugin_config: PluginOptions = serde_json::from_str(
        &metadata
            .get_transform_plugin_config()
            .expect("Failed to get plugin config for @graphql-codegen/client-preset-swc-plugin"),
    )
    .expect("Invalid configuration for @graphql-codegen/client-preset-swc-plugin");

    let artifact_directory = plugin_config.artifactDirectory;
    if artifact_directory.is_empty() {
        panic!("artifactDirectory is not present in the config for @graphql-codegen/client-preset-swc-plugin");
    }

    let visitor = create_graphql_codegen_visitor(GraphQLCodegenOptions {
        filename,
        cwd,
        artifact_directory,
        gql_tag_name: plugin_config.gqlTagName,
    });

    program.apply(&mut visit_mut_pass(visitor))
}
