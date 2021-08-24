module OperationalTransformation
  ( Operation,
    OperationSeq,
    fromList,
    empty,
    add,
    addDelete,
    addInsert,
    addRetain,
    apply,
  )
where

import OperationalTransformation.Internal