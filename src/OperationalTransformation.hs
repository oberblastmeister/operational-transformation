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
    apply',
    invert,
    compose,
    transform,
    fromChanges,
    fromChanges',
    fromChangesUnchecked,
  )
where

import OperationalTransformation.Internal