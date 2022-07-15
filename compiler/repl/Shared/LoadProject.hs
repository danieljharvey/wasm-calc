module Shared.LoadProject
  ( loadProject,
  )
where

import Control.Monad.Except
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import qualified ReplNew.Persistence as Repl
import ReplNew.ReplM

loadProject ::
  ReplM
    (Error Annotation)
    (Either (Error Annotation) (Project Annotation))
loadProject =
  do
    env <- mapError StoreErr Repl.loadProject
    pure (Right env)
    `catchError` (pure . Left)