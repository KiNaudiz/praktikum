import Text.Printf

u_CE,i_B,u_BE,i_C :: Double
u_CE    = 5.0 -- V
i_B     = 0.065 -- mA
u_BE    = 0.65 -- V
i_C     = 5.5 -- mA

i_q',u_RE',u_B' :: Double
-- i_q = (3..10) i_B
i_q'    = 5*i_B
-- u_RE = (0.5..1) V
u_RE'   = 1
u_B'    = 10 -- V

main :: IO ()
main = do
    let
        (r_1,r_2,r_C,r_E) = calcR i_q' u_RE' u_B'
    _ <- putStrLn $ "R_1 = " ++ printf "%9.2f" r_1 ++ " Ohm"
    _ <- putStrLn $ "R_2 = " ++ printf "%9.2f" r_2 ++ " Ohm"
    _ <- putStrLn $ "R_C = " ++ printf "%9.2f" r_C ++ " Ohm"
    _ <- putStrLn $ "R_E = " ++ printf "%9.2f" r_E ++ " Ohm"
    return()

calcR :: Double -> Double -> Double -> (Double,Double,Double,Double)
calcR i_q u_RE u_B = (r_1,r_2,r_C,r_E)
    where
        i_E     = i_B + i_C
        i_R2    = i_q - i_B
        u_R2    = u_BE + u_RE
        u_R1    = u_B - u_R2
        u_RC    = u_B - u_CE - u_RE

        r_1     = u_R1/(i_q*10**(-3)) :: Double
        r_2     = u_R2/(i_R2*10**(-3)) :: Double
        r_C     = u_RC/(i_C*10**(-3)) :: Double
        r_E     = u_RE/(i_E*10**(-3)) :: Double
