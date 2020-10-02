import Resolution
-- Test
main = do
    putStrLn $ show (resX([[Literal True "B", Literal False "C"], [Literal False "B", Literal True "A"], [Literal False "B", Literal True "C"], [Literal False "A", Literal False "C"], [Literal False "C", Literal True "B"]]))
    putStrLn $ show (resX([[Literal True "A", Literal True "B", Literal False "C"], [Literal False "B"], [Literal True "B", Literal True "C", Literal True "A"], [Literal True "C"] ,[Literal False "A", Literal False "C"]]))