module Helpers where

removeDuplicates : List a -> List a
removeDuplicates list =
  case list of
    [] ->
      []

    l :: ls ->
      if (List.member l ls) then
        (removeDuplicates ls)
      else
        (l :: (removeDuplicates ls))


isJust : Maybe a -> Bool
isJust a =
  case a of
    Just _ ->
      True

    _ ->
      False


unsafe : Maybe a -> a
unsafe v =
  case v of
    Just x ->
      x

    Nothing ->
      Debug.crash "unsafe with Nothing"

zip : List a -> List b -> List ( a, b )
zip =
  List.map2 (,)


tuples : List a -> List b -> List ( a, b )
tuples xs ys =
  List.concatMap (\x -> (List.map (\y -> ( x, y )) ys)) xs

fpow : Int -> (a -> a) -> a -> a
fpow n f a = case n of
        0 -> a
        _ -> f (fpow (n-1) f a)
