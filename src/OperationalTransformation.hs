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
    compose,
    transform,
    fromChanges,
  )
where

import OperationalTransformation.Internal