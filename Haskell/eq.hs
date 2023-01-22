Class Eq a where
  equals :: a -> a -> Bool

Instance Eq Int where
  equals x y = primIntEq
Instance Eq Day where
  equals Mon Mon = True
  equals _  _ = False