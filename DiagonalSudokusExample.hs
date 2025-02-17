module DiagonalSudokusExample where
type Grid = Matrix Char
type Matrix a = [Row a] 
type Row a = [a]
type Choices = [Char]
easy::Grid
easy = ["4--167--9",
        "-6--4--2-",
        "--8---6--",
        "9---2---6",
        "58-6-9-71",
        "6---8---5",
        "--1---5--",
        "-7--1--9-",
        "8--376--2"]

gentle::Grid  --NO SOLUTION
gentle = ["-842--7--",
          "7--3-----",
          "----879-4",
          "8--4--69-",
          "-1--3--2-",
          "-46--9--5",
          "6-197----",
          "-----4--9",
          "--8--316-"]

diabolical::Grid --VERIFY
diabolical = ["----5678-",
              "1-----3--",
              "27-3-----",
              "3-----9--",
              "4-------2",
              "--9-----3",
              "-----7-54",
              "--2-----8",
              "-4563----"]


unsolvable::Grid
unsolvable = ["------8--",
              "-----19--",
              "43--87---",
              "-21------",
              "--5---6--",
              "------45-",
              "---87--65",
              "--39-----",
              "--4------"]
