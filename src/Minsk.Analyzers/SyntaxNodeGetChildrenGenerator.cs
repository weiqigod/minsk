using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;

namespace Minsk.Analyzers
{
    [Generator]
    public class SyntaxNodeGetChildrenGenerator : ISourceGenerator
    {
        public void Initialize(InitializationContext context)
        {
        }

        public void Execute(SourceGeneratorContext context)
        {
            var compilation = (CSharpCompilation)context.Compilation;
            var types = GetAllTypes(compilation.Assembly);

            var syntaxNodeType = compilation.GetTypeByMetadataName("Minsk.CodeAnalysis.Syntax.SyntaxNode");
            var syntaxTokenType = compilation.GetTypeByMetadataName("Minsk.CodeAnalysis.Syntax.SyntaxToken");
            var separatedSyntaxListType = compilation.GetTypeByMetadataName("Minsk.CodeAnalysis.Syntax.SeparatedSyntaxList`1");
            var immutableArrayType = compilation.GetTypeByMetadataName("System.Collections.Immutable.ImmutableArray`1");          

            var syntaxTypes = types.Where(t => IsDerivedFrom(t, syntaxNodeType));

            var sb = new StringBuilder();
            sb.AppendLine($"namespace Minsk.CodeAnalysis.Syntax");
            sb.AppendLine("{");

            foreach (var type in syntaxTypes)
            {
                if (type.IsAbstract)
                    continue;

                if (SymbolEqualityComparer.Default.Equals(type, syntaxTokenType))
                    continue;
                
                var ctor = type.Constructors.OrderBy(c => c.Parameters.Length).LastOrDefault();
                if (ctor == null)
                    continue;

                sb.AppendLine($"    partial class {type.Name}");
                sb.AppendLine("    {");
                sb.AppendLine("        public override System.Collections.Generic.IEnumerable<Minsk.CodeAnalysis.Syntax.SyntaxNode> GetChildren()");
                sb.AppendLine("        {");

                foreach (var parameter in ctor.Parameters)
                {
                    var propertyName = parameter.Name.Substring(0, 1).ToUpper() + parameter.Name.Substring(1);

                    if (IsImmutableArray(immutableArrayType, parameter.Type))
                    {
                        sb.AppendLine($"            foreach (var child in {propertyName})");
                        sb.AppendLine($"                yield return child;");
                    }
                    else if (IsSeparatedSyntaxList(separatedSyntaxListType, parameter.Type))
                    {
                        sb.AppendLine($"            foreach (var child in {propertyName}.GetWithSeparators())");
                        sb.AppendLine($"                yield return child;");
                    }
                    else if (IsDerivedFrom(parameter.Type, syntaxNodeType))
                    {
                        sb.AppendLine($"            yield return {propertyName};");
                    }
                }

                sb.AppendLine("        }");
                sb.AppendLine("    }");
            }

            sb.AppendLine("}");

            // Normally, we'd just return the source via:
            //
            //      context.AddSource(outputPath, SourceText.From(sb.ToString(), Encoding.UTF8));
            //
            // However, VS/VS Code don't understand generators yet, so I'm emitting the file into
            // the source tree. Not nice, but workable for now.

            var syntaxNodeFilePath = syntaxNodeType.DeclaringSyntaxReferences.First().SyntaxTree.FilePath;
            var syntaxNodeDirectoryPath = Path.GetDirectoryName(syntaxNodeFilePath);
            var outputPath = Path.Combine(syntaxNodeDirectoryPath, "SyntaxNode_GetChildren.cs");
            var newSource = sb.ToString();

            var oldSource = File.ReadAllText(outputPath);

            if (newSource != oldSource)
            {
                File.WriteAllText(outputPath, newSource);                       
                context.AddSource(outputPath, SourceText.From("#error MINSK: Generator code changed. Please recompile.", Encoding.UTF8));
            }
        }

        private static bool IsImmutableArray(INamedTypeSymbol immutableArrayType, ITypeSymbol type)
        {
            return type is INamedTypeSymbol named &&
                   named.TypeArguments.Length == 1 &&
                   SymbolEqualityComparer.Default.Equals(named.OriginalDefinition, immutableArrayType);
        }

        private bool IsSeparatedSyntaxList(INamedTypeSymbol separatedSyntaxListType, ITypeSymbol type)
        {
            return type is INamedTypeSymbol named &&
                   named.TypeArguments.Length == 1 &&
                   SymbolEqualityComparer.Default.Equals(named.OriginalDefinition, separatedSyntaxListType);
        }

        private static IEnumerable<INamedTypeSymbol> GetAllTypes(IAssemblySymbol assembly)
        {
            var stack = new Stack<INamespaceSymbol>();
            stack.Push(assembly.GlobalNamespace);

            while (stack.Count > 0)
            {
                var ns = stack.Pop();

                foreach (var t in ns.GetTypeMembers())
                    yield return t;

                foreach (var cns in ns.GetNamespaceMembers())
                    stack.Push(cns);
            }
        }

        private static bool IsDerivedFrom(ITypeSymbol t, INamedTypeSymbol baseType)
        {
            if (t is INamedTypeSymbol type)
            {
                while (type != null)
                {             
                    if (SymbolEqualityComparer.Default.Equals(type, baseType))
                        return true;

                    type = type.BaseType;
                }
            }

            return false;
        }
    }
}
