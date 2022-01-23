module Main where
import Data.Ratio ((%))
import Debug.Trace
import System.Random
import Data.List (unfoldr, cycle)
import Euterpea

type Step = (Dur, Bool)
data Rythm = Rythm { _n :: Int, _k :: Int, _steps :: [Step] }
type Beat = Dur -> Music Pitch
type Tracker = [Rythm]

rythm :: Int -> Int -> Rythm
rythm n' k' = terminate $ rythm' (n', k' , replicate k' [True] <> replicate (n' - k') [False])
  where
    terminate (_, _, xs) = Rythm n' k' $ zip (repeat $ 1 % fromIntegral n') (concat xs)
    
    rythm' :: (Int, Int, [[Bool]]) -> (Int, Int, [[Bool]])
    rythm' (n, k, xs)
      | k == n || m == 1 = (n, k, xs)
      | otherwise = rythm' (max k m, min k m, euclidean)
      where
        m = n - k
        (q, r) = splitAt k xs
        
        euclidean = take n $ zipWith (<>) (q <> repeat []) (r <> repeat [])

sequencer :: Rythm -> [Beat] -> Music Pitch
sequencer rm = f' (cycle $ _steps rm)
  where
    f' ((k ,True):xs) (y:ys) = y k :+: f' xs ys 
    f' ((k, False):xs) ys = rest k :+: f' xs ys
    f' _ [] = rest 0

tracker :: [(Rythm, [Beat])] -> Music Pitch
tracker = foldr f' (rest 0)
  where
    f' trk m = uncurry sequencer trk :+: m

legato :: Rythm -> Rythm
legato r = r { _steps = f' (_steps r) }
  where
    f' :: [Step] -> [Step]
    f' [] = []
    f' ((_, False):xs) = f' xs
    f' ((k, True):(k', False):xs) = f' $ (k + k', True) : xs
    f' (x:xs) = x : f' xs

rotate :: Rythm -> Rythm
rotate r = r { _steps = f' . _steps $ r }
  where
    f' [] = []
    f' (x:xs) = xs <> [x]

printRythm :: Rythm -> String
printRythm = foldr f' "" . _steps
  where
    f' (_, True) acc = 'x' : acc
    f' (_, False) acc = '.' : acc



-- d e f g a bf
track4 :: Music Pitch
track4 =
  cut 24 intro
  :+:
  cut 24 h_break
  :+:
  cut 38 part1
  :+:
  cut 24 h_break
  :+:
  cut 38 part2
  
  where
    h_break =
      sequencer (rythm 14 8) (repeat $ perc AcousticBassDrum)
      :=:
      (forever . offset (1 % 28) . tracker $ [ (rythm 28 4, replicate 8 $ perc AcousticSnare)
                                             , (rythm 28 4, replicate 8 $ perc AcousticSnare)
                                             , (rythm 28 4, replicate 8 $ perc AcousticSnare)
                                             , (rythm 28 16, replicate 8 $ perc AcousticSnare)
                                             ])
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 7 4) $ cycle [ d 3, d 3, d 3, d 3
                                                                            , a 2, a 2, a 2, a 2
                                                                            , bf 2, bf 2, bf 2, bf 2
                                                                            ])
      :=:
      (offset 0 . instrument OverdrivenGuitar $ sequencer (rythm 14 8) $
       cycle [ f 4, f 4, f 4, f 4, d 4, d 4, d 4, d 4
             , bf 3, bf 3, bf 3, bf 3, g 3, g 3, g 3, g 3
             , a 3, a 3, a 3, a 3, d 4, d 4, d 4, d 4
             ])

      :=:
      (offset 6 . instrument DistortionGuitar $ sequencer (legato $ rythm 14 4) $
        cycle [ f 3, f 3, f 3, f 3, d 3, d 3, d 3, d 3
              , bf 3, bf 3, bf 3, bf 3, g 3, g 3, g 3, g 3
              , a 3, a 3, a 3, a 3, d 4, d 4, d 4, d 4
              ])

    intro =
      sequencer (rotate $ rythm 14 14) (cycle [perc ClosedHiHat, perc PedalHiHat])
      :=:
      sequencer (rythm 14 7) (repeat $ perc OpenHiHat)
      :=:
      sequencer (rythm 14 8) (repeat $ perc AcousticBassDrum)
      :=:
      sequencer (rotate $ rythm 7 4) (repeat $ perc ElectricSnare)
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 14 6) $
       cycle [ d 2, d 2, d 2, d 2, d 2, rest
             , f 2, f 2, f 2, f 2, f 2, rest
             , e 2, e 2, e 2, e 2, e 2, e 2
             , d 2, d 2, d 2, d 2, d 2, rest
             , g 2, g 2, g 2, g 2, g 2, rest
             , f 2, f 2, f 2, f 2, f 2, f 2
             ])
      :=:
      (offset 12 . instrument GuitarHarmonics $ sequencer (rotate $ rythm 7 1) $ cycle [ d 3
                                                                                       , f 3
                                                                                       , e 4
                                                                                       , d 3
                                                                                       , g 3
                                                                                       , f 4
                                                                                       ])
      :=:
      (offset 18 $ sequencer (rotate $ rythm 14 6) $ repeat . perc $ AcousticBassDrum)
      :=:
      (offset 18 $ sequencer (rotate . rotate $ rythm 7 2) $ repeat . perc $ AcousticSnare)

    part1 =
      (sequencer (rythm 14 14) $ cycle [perc ClosedHiHat, perc PedalHiHat])
      :=:
      (forever . offset (1%7) $ tracker [ (rythm 7 4, take 12 . cycle $ [perc ElectricSnare, perc AcousticSnare])
                                        , (rythm 7 5, take 5 . cycle $ [perc ElectricSnare])
                                        ])
      :=:
      (sequencer (rythm 14 7) $ repeat $ perc AcousticBassDrum)
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 14 8) $ cycle [f 2, f 2, f 2, f 2
                                                                             , a 2, bf 2, bf 2, bf 2
                                                                             , e 2, e 2, e 2, g 2
                                                                             ])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rythm 14 10) $ take (45*4) . cycle $ [ d 4, d 4, d 5, d 4, d 4
                                                                                     , f 4, f 4, f 4, f 4, f 4
                                                                                     , d 4, d 4, d 5, d 4, d 4
                                                                                     , e 4, e 4, e 4, e 4 , e 4
                                                                                     , g 4, g 4, g 5, g 4, g 4
                                                                                     , bf 4, bf 4, bf 5, bf 4
                                                                                     , c 5, rest, c 5, rest
                                                                                     , a 4, rest, a 4, rest
                                                                                     , f 4, d 4, f 4, d 4, f 4
                                                                                     ])
      :+:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 14 5) $ cycle [ g 4, g 4, g 4, d 4, bf 3
                                                                             , d 4, c 4, d 4, c 4, d 4
                                                                             , e 4, e 4, e 4, e 4, a 3
                                                                             , f 4, f 4, f 4, c 4, d 4
                                                                             ])

    part2 =
      (sequencer (rythm 14 10) $ cycle [perc ClosedHiHat, perc PedalHiHat])
      :=:
      (sequencer (rotate $ rythm 14 4) $ repeat $ perc OpenHiHat)
      :=:
      (sequencer (rythm 14 9) $ repeat $ perc AcousticBassDrum)
      :=:
      (offset (1%14) $ sequencer (rythm 14 5) $ repeat $ perc AcousticSnare)
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 14 12) $ cycle [ d 3, d 3, d 3, d 3, d 3, d 3
                                                                              , c 3, c 3, c 3, c 3, c 3, c 3
                                                                              , e 3, e 3, e 3, e 3, e 3, e 3
                                                                              , d 3, d 3, d 3, d 3, d 3, d 3
                                                                              , f 3, f 3, f 3, f 3, f 3, f 3
                                                                              , e 3, e 3, e 3, e 3, e 3, e 3
                                                                              ])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rythm 7 6) $ take 72 . cycle $ [ bf 4, bf 4, bf 4
                                                                               , g 4, g 4, g 4
                                                                               , a 4, a 4, a 4
                                                                               ])
      :+:
      (instrument OverdrivenGuitar $ sequencer (rythm 7 7) $ cycle [ f 4, f 4, e 4
                                                                   , f 4, f 4, e 4
                                                                   , f 4, f 4, e 4
                                                                   , g 4, g 4, e 4
                                                                   , f 4, f 4, e 4
                                                                   , f 4, f 4, e 4
                                                                   , f 4, f 4, e 4
                                                                   , c 4, c 4, e 4
                                                                   ])
      :=:
      (instrument DistortionGuitar . offset 24 $ sequencer (rythm 7 7) $ cycle [ f 4, f 4, e 4
                                                                               , f 4, f 4, e 4
                                                                               , f 4, f 4, e 4
                                                                               , g 4, g 4, e 4
                                                                               , f 4, f 4, e 4
                                                                               , f 4, f 4, e 4
                                                                               , f 4, f 4, e 4
                                                                               , c 4, c 4, e 4
                                                                               ])
  
-- f g af bf c df ef

track3 :: Music Pitch
track3 =
  cut (27+9) intro
  :+:
  cut (27+9) verse1
  :+:
  cut (27*2) breakbeat1
  :+:
  cut (27+18) (verse1 :=: harmony)
  
  where
    intro =
      sequencer (rythm 18 18) (cycle $ [perc ClosedHiHat, perc PedalHiHat])
      :=:
      (offset 9 $ sequencer (rythm 18 9) (cycle $ [perc AcousticBassDrum]))
      :=:
      (offset 18 . forever . tracker $
       [ (rythm 18 9, take 18 . cycle $ [rest, perc AcousticSnare])
       , (rythm 18 12, take 12 . cycle $ [perc AcousticSnare, perc ElectricSnare, perc ElectricSnare])
       ])
      :=:
      (instrument DistortionGuitar . forever . tracker $
       [ (legato $ rythm 11 4, [f 2, f 2, af 2, af 2, g 2, g 2])
       , (rythm 8 8, [c 3, c 3, c 3, df 3, df 3, bf 2, bf 2, bf 2])
       , (legato $ rythm 8 6, [af 2, af 2, af 2, g 2, g 2, g 2])
       ])
      :=:
      (offset 27 ((instrument OverdrivenGuitar . forever $ sequencer (legato $ rythm 18 9) $
                   cycle [ af 3, rest, af 3,
                           f 3, rest, f 3,
                           g 3, rest, g 3,
                           c 4, rest, c 4,
                           bf 3, bf 3, bf 3,
                           g 3, g 3, g 3,
                           af 3, af 3, af 3,
                           c 4, c 4, c 4,
                           f 3, f 3, f 3
                         ])))

    verse1 =
      sequencer (rotate $ rythm 18 14) (repeat $ perc ClosedHiHat)
      :=:
      sequencer (rythm 18 4) (repeat $ perc OpenHiHat)
      :=:
      sequencer (rythm 18 10) (repeat $ perc AcousticBassDrum)
      :=:
      sequencer (rotate $ rythm 9 4) (repeat $ perc AcousticSnare)
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 18 8) $
        cycle [ g 2, g 2, g 2, g 2
              , df 3, df 3, df 3, df 3
              , c 3, c 3, c 3, c 3
              , bf 2, bf 2, bf 2, bf 2
              , g 2, g 2, g 2, g 2
              , df 3, df 3, df 3, df 3
              , c 3, c 3, c 3, c 3
              , bf 2, bf 2, bf 2, bf 2
              ])
      :=:
      (instrument OverdrivenGuitar . forever $ tracker
       [ (rythm 9 8, [ af 3, f 3, c 4, af 3
                     , g 3, df 3, ef 3, g 3
                     , f 3, f 3, f 3, f 3
                     ])
       , (legato $ rythm 9 5, [ g 3, g 3, g 3, af 3, bf 3 ])
       , (legato $ rythm 9 7, [ df 4, df 4, c 4, c 4, c 4, bf 3, bf 3, bf 3 ])
       
       , (rotate . legato $ rythm 9 8, [ af 3, f 3, c 3, af 3
                                       , g 3, c 3, c 4, g 3
                                       ])
       , (legato $ rythm 9 5, take 20 . cycle $ [ ef 3, ef 3, df 3, f 3, f 3
                                                , g 3, g 3, af 3, ef 3, ef 3
                                                ])
       ])

    breakbeat1 =
      (sequencer (rythm 18 18) (cycle $ [perc ClosedHiHat, perc PedalHiHat]))
      :=:
      (sequencer (rythm 18 17) (repeat $ perc AcousticBassDrum))
      :=:
      (sequencer (rotate $ rythm 9 7) (repeat $ perc AcousticSnare))
      :=:
      (forever . tempo 0.75 . instrument OverdrivenGuitar $ tracker
       [ (legato $ rythm 18 13, replicate 10 $ f 3)
       , (legato $ rythm 18 15, replicate 15 $ c 3)
       , (legato $ rythm 18 17, replicate 17 $ af 2)
       , (legato $ rythm 9 7, replicate 7 $ f 2)
       ])
      :=:
      (offset 18 . instrument DistortionGuitar $ sequencer (legato $ rythm 9 8) $ cycle [ af 3, af 3, af 3, af 3
                                                                                        , bf 3, bf 3, bf 3, bf 3
                                                                                        , g 3, g 3, g 3, g 3
                                                                                        , f 3, f 3, f 3, f 3
                                                                                        , af 3, af 3, af 3, af 3
                                                                                        , bf 3, bf 3, bf 3, bf 3
                                                                                        , df 4, df 4, df 4, df 4
                                                                                        , c 4, c 4, c 4, c 4
                                                                                        ])
      :=:
      (offset (27+18) . instrument OverdrivenGuitar $ sequencer (legato $ rythm 9 3) $ cycle [ af 4, bf 4, af 4
                                                                                             , bf 4, f 4, g 4
                                                                                             , f 4, g 4, f 4
                                                                                             , c 5, af 4, c 5
                                                                                             , af 4, c 5, bf 4
                                                                                             ])

    harmony =
      (instrument SynthStrings1 $ sequencer (rythm 18 8) $ cycle [ af 4, af 4, af 4, af 4
                                                                 , bf 4, bf 4, bf 4, bf 4
                                                                 , g 4, g 4, g 4, g 4
                                                                 , f 4, f 4, f 4, f 4
                                                                 , af 4, af 4, af 4, af 4
                                                                 , bf 4, bf 4, bf 4, bf 4
                                                                 , df 5, df 5, df 5, df 5
                                                                 , c 5, c 5, c 5, c 5
                                                                 ])
      :=:
      (offset 18 . instrument GuitarHarmonics $ sequencer (rotate $ rythm 18 10) (repeat $ c 4))
      
  
track2 :: Music Pitch
track2 =
  cut 24 intro
  :+:
  cut 32 verse1
  :+:
  cut 11 breakbeat1
  :+:
  cut 32 verse1
  
  where
    intro =
      sequencer (rythm 12 12) (repeat $ perc ClosedHiHat)
      :=:
      (offset 10 $ sequencer (rythm 12 6) (repeat $ perc BassDrum1))
      :=:
      (offset 15 $ tracker . zip (cycle [rythm 6 3, rythm 6 3, rythm 6 5]) $ repeat (replicate 5 $ perc ElectricSnare))
      :=:
      (instrument OverdrivenGuitar . offset 20 $ sequencer (rythm 12 10) $ cycle [d 4])
      :=:
      (instrument DistortionGuitar . sequencer (legato $ rythm 6 5) $
       cycle [ f 3, d 3, a 3, d 3, d 3
             , e 3, c 3, g 3, c 3, e 3
             , f 3, d 3, a 3, d 3, d 3
             , g 3, c 3, e 3, c 3, d 3
             ]
      )

    verse1 =
      sequencer (rotate $ rythm 12 8) (repeat $ perc ClosedHiHat)
      :=:
      sequencer (rythm 12 4) (repeat $ perc OpenHiHat)
      :=:
      sequencer (rythm 12 5) (repeat $ perc BassDrum1)
      :=:
      sequencer (rotate $ rythm 12 4) (repeat $ perc ElectricSnare)
      :=:
      (instrument DistortionGuitar $ sequencer (rythm 12 6) $
        cycle [ d 3, d 3, d 3
              , f 3, f 3, f 3
              , a 3, a 3, a 3
              , bf 3, bf 3, bf 3
              , g 3, g 3, g 3
              , a 3, a 3, a 3
              ])
      :=:
      (instrument DistortionGuitar $ sequencer (rotate $ rythm 12 6) $
        cycle [ bf 2, bf 2, bf 2
              , d 2, d 2, d 2
              , e 2, e 2, e 2
              , f 2, f 2, f 2
              , e 2, e 2, e 2
              ])
      :=:
      (forever $ (instrument OverdrivenGuitar . cut 13 $ tracker . cycle $
                  [ (rythm 6 4, [d 4, f 4, d 4, g 4])
                  , (legato $ rythm 6 4, [a 4, bf 4, a 4, bf 4, g 4])
                  , (rythm 6 5, [f 4, g 4, bf 4, a 4])
                  
                  , (rythm 6 4, [d 4, f 4, d 4, g 4])
                  , (legato $ rythm 6 4, [a 4, bf 4, a 4, bf 4, c 5, a 4])
                  ])
      :+:
      (instrument DistortionGuitar . cut 9 $ sequencer (rotate $ rythm 6 3) $
        cycle [ f 4, f 4, f 4
              , d 4, d 4, d 4
              , e 4, e 4, e 4
              , g 4, g 4, g 4
              ]))

    breakbeat1 =
      sequencer (rythm 11 11) (cycle [perc ClosedHiHat, perc PedalHiHat])
      :=:
      sequencer (rythm 11 8) (cycle [perc BassDrum1])
      :=:
      sequencer (rotate $ rythm 11 5) (cycle [perc ElectricSnare])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rythm 11 10) $
       cycle [ d 3, d 3, d 3, d 3, d 3
             , c 3, c 3, c 3, c 3, c 3
             , e 3, e 3, e 3, e 3, e 3
             , f 3, f 3, f 3, f 3, f 3
             ])
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 11 4) $
        cycle [ bf 2, d 3, bf 2, d 3
              , a 2, c 3, a 2, c 3
              , g 2, bf 2, g 2, bf 2
              , a 2, c 3, a 2, c 3
              ])
      
track1 :: Music Pitch
track1 =
  cut 16 intro
  :+:
  cut 4 breakbeat1
  :+:
  cut 21 part1
  :+:
  cut 4 breakbeat2
  :+:
  cut 57 part2
  where
    intro :: Music Pitch
    intro =
      (sequencer (rythm 8 8) $ cycle [perc ClosedHiHat, perc PedalHiHat])
      :=:
      (sequencer (rythm 8 4) $ repeat . perc $ BassDrum1)
      :=:
      (offset 8 $ sequencer (rotate $ rythm 8 3) $ repeat . perc $ ElectricSnare)
      :=:
      instrument DistortionGuitar (sequencer (legato $ rythm 8 6) $ cycle [e 2, g 2, b 2, d 2, fs 2, a 2])
      :=:
      instrument OverdrivenGuitar (sequencer (rythm 8 3) $ cycle [e 3, e 3, fs 3, e 3, e 3, g 3])
      :=:
      instrument OverdrivenGuitar (sequencer (rotate $ rythm 8 5) $ cycle [e 2, e 3, e 2, b 2, g 2])

    breakbeat1 :: Music Pitch
    breakbeat1 =
      (sequencer (rotate $ rythm 16 14) $ repeat . perc $ ClosedHiHat)
      :=:
      (sequencer (rythm 16 2) $ repeat . perc $ OpenHiHat)
      :=:
      (sequencer (rythm 16 8) $ repeat . perc $ ElectricSnare)
      :=:
      (sequencer (rotate $ rythm 16 8) $ repeat . perc $ AcousticBassDrum)
      :=:
      (instrument OverdrivenGuitar $ sequencer (rythm 8 3) $ cycle [e 3, e 3, fs 3, e 3, e 3, g 3])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rotate $ rythm 8 5) $ cycle [e 2, e 3, e 2, b 2, g 2])
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 16 8) $ cycle [fs 2, g 2, fs 2, e 2, g 2, e 2])

    breakbeat2 :: Music Pitch
    breakbeat2 =
      (sequencer (rotate $ rythm 16 16) $ repeat . perc $ ClosedHiHat)
      :=:
      (sequencer (rythm 16 2) $ repeat . perc $ OpenHiHat)
      :=:
      (sequencer (rotate $ rythm 16 8) $ repeat . perc $ ElectricSnare)
      :=:
      (sequencer (rythm 16 8) $ repeat . perc $ AcousticBassDrum)
      :=:
      (instrument OverdrivenGuitar $ sequencer (rythm 16 8) $ cycle [fs 4, g 4, fs 4, e 4, g 4, e 4])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rotate $ rythm 16 8) $ cycle [e 4])
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 8 5) $ cycle [fs 2, fs 2, fs 2, g 2, g 2, g 2])

    part1 :: Music Pitch
    part1 =
      (sequencer (rythm 16 16) $ repeat . perc $ ClosedHiHat)
      :=:
      (sequencer (rotate $ rythm 16 4) $ repeat . perc $ ElectricSnare)
      :=:
      (sequencer (rotate $ rythm 16 2) $ repeat . perc $ OpenHiHat)
      :=:
      (sequencer (rythm 16 8) $ repeat . perc $ AcousticBassDrum)
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 8 5) $ cycle [g 2, g 2, g 2, g 2, g 2, e 2, e 2, e 2, e 2, e 2, fs 2, fs 2, fs 2, fs 2, fs 2])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rotate $ rythm 16 10) $ cycle [e 4, g 4, b 4, g 4, b 4, d 4])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rythm 16 6) $ cycle [c 4, e 4, g 4, fs 4, a 4, c 5])
      :=:
      ((instrument SynthBass1 . offset 14 $
        sequencer (legato $ rythm 16 10) (cycle [g 3, g 3, g 3, g 3, g 3, e 3, e 3, e 3, e 3, e 3, fs 3, fs 3, fs 3, fs 3, fs 3])))


    part2 :: Music Pitch
    part2 =
      (sequencer (rythm 16 16) $ repeat . perc $ ClosedHiHat)
      :=:
      (sequencer (rotate $ rythm 16 5) $ repeat . perc $ ElectricSnare)
      :=:
      (sequencer (rythm 16 11) $ repeat . perc $ AcousticBassDrum)
      :=:
      (instrument DistortionGuitar $ sequencer (rotate $ rythm 16 11) $ cycle [g 3, g 3, g 3, g 3, g 3, e 3, e 3, e 3, e 3, e 3, fs 3, fs 3, fs 3, fs 3, fs 3])
      :=:
      (instrument DistortionGuitar $ sequencer (legato $ rythm 16 5) $ cycle [b 2, b 2, b 2, b 2, b 2, g 2, g 2, g 2, g 2, c 3, c 3, c 3, c 3, c 3])
      :=:
      (instrument OverdrivenGuitar $ sequencer (rythm 8 7) (cycle [b 4, b 4, fs 4, fs 4, g 4, g 4]))
      :=:
      (offset 14 . instrument OverdrivenGuitar $ sequencer (legato $ rythm 8 7) (cycle [b 4, b 4, fs 4, fs 4, g 4, g 4]))
      :=:
      (offset 28 . instrument DistortionGuitar $ sequencer (rythm 16 16) $ cycle $ replicate 16 (e 5) <> replicate 16 (c 4) <> replicate 16 (d 4) <> replicate 16 (e 4) <> replicate 16 (e 5) <>  replicate 16 (d 4) <> replicate 16 (c 4) <> replicate 16 (e 4))

main :: IO()
main = putStrLn "0 player band"
