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
    invert,
  )
where

import OperationalTransformation.Internal