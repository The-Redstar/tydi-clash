{-# LANGUAGE MultiParamTypeClasses #-}


module Tydi.Connect where

-- utilities for connecting pstreams and logical streams
-- TODO

class Connect p q where
  connect :: p -> q



-- instance for isomorphic types
-- instance for increasing complexity levels

-- instance for decreasing ready signal complexity level
