
module ModuleSystem.Namespace(
         Namespace, makeNamespace, emptyNamespace,
         unresolveQName,
         Associativity(..), Precedence(..),
         associativityAndPrecedence
       ) where

import Syntax.Name(QName(..))
import ModuleSystem.Module(Module, emptyModule)
import ModuleSystem.Context(Context, emptyContext, unresolveName)
import ModuleSystem.PrecedenceTable(
         PrecedenceTable, emptyPrecedenceTable,
         Associativity(..), Precedence(..),
         isOperator, operatorAssociativity, operatorPrecedence
       )

data Namespace = NS {
                   nsModule          :: Module
                 , nsContext         :: Context
                 , nsPrecedenceTable :: PrecedenceTable
                 }

makeNamespace :: Module -> Context -> PrecedenceTable -> Namespace
makeNamespace = NS

emptyNamespace :: Namespace
emptyNamespace = NS emptyModule
                    (emptyContext (Name "(...)"))
                    emptyPrecedenceTable

unresolveQName :: Namespace -> QName -> QName
unresolveQName ns qname = unresolveName (nsModule ns) (nsContext ns) qname

associativityAndPrecedence :: Namespace -> QName
                           -> Maybe (Associativity, Precedence)
associativityAndPrecedence ns qname =
  let precedenceTable = nsPrecedenceTable ns in
    if isOperator qname precedenceTable
     then Just (operatorAssociativity qname precedenceTable,
                operatorPrecedence qname precedenceTable)
     else Nothing

