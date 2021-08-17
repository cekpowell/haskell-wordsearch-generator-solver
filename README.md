# Wordsearch Generator and Solver
## COMP2209: Programming III
---

### Task Description
- Write programs that can be used for the generation and solving of wordsearch puzzles using Haskell.

---

### Generator

#### Usage

- Contained in `Generator/build` is a `generator` executable which can be used to **generate wordsearches**.

- The generator can be **run** with the following command:

  - ```bash
    ./generator <word-list> <density>
    ```

  - Where:

    - `word-list` is a comma-seperated list of words that should appear within the wordsearch.
    - `density` is a double between 0 and 1 (*exclusive*) that represents the ratio between letters that are contained within words and filler words (i.e., the lower the density, the harder the words are to find).
    - There is **no error checking** on the run-time parameters, so they must be correct.

- If the executable is not functioning, it can be rebuilt using **Cabal**. 

- Navigate to the `Generator` directory, and run the following command to **build** the executable:

  - ```
    cabal build
    ```

- Use the following command to **run** the executable:

  - ```
    cabal run generator <word list> <density>
    ```

#### Output

- The produced wordsearch will:
  - Contain each word exactly once, at a random position and orientation.
  - Contain only letters contained within the provided wordlist.

- <u>*Example:*</u>

  - **Input**:

    - ```
      ./generator bob,jane,tom,frank,ben,bill 0.3
      ```

  - **Output**:

    - ```
      F K J B O E O E B 
      A B O I E B E N O 
      N K T N L L T J L 
      N E R E E T R O B 
      A F B O F K O E E 
      B R B O L R R M N 
      I A N O B B A E I 
      L J A N E E N N E 
      L R E E F N E T K 
      ```

- <u>*Example:*</u>

  - **Input**:

    - ```
      cabal build 
      cabal run generator television,monitor,speaker,telephone,house,garage 0.2
      ```

  - **Output**:

    - ```
      H H S U O R S V R T E R P E L 
      E R E N O H P E L E T O I E A 
      G I L I A O O I R G T U N E I 
      O M O G R I I O H E E S E I M 
      E R O P P R T N L I E A G P R 
      O T A H E I I E L G P E O L O 
      S P P H N H V A P N I S R S N 
      I A O O S I E I E O I T G S O 
      O A M K S L O S S I V A K E G 
      E K G I M E O E H P N H L L A 
      P E O T O E S O E O E P E S R 
      H N N S E R U N V I S A E E A 
      S G T G E S P S L O S S K A G 
      A O N I E T R E E K N E E E E 
      T O U P E A P R G L E S I R R
      ```

---

### Solver

#### Usage

- Contained in `Solver/build` is a `solver` executable which can be used to **solve** **wordsearches**.

- The solver can be **run** with the following command:

  - ```bash
    ./solver <word-list> <grid-list>
    ```

  - Where:

    - `word-list` is a comma-seperated list of words that are being **searched for** within the wordsearch grid.
    - `grid-list` is a comma-seperated list of the **rows** within the grid - each row must be given as a connected string of characters (i.e., no whitespace between cells).
    - There is **no error checking** on the run-time parameters, so they must be correct.

- If the executable is not functioning, it can be rebuilt using **Cabal**. 

- Navigate to the `Generator` directory, and run the following command to **build** the executable:

  - ```
    cabal build
    ```

- Use the following command to **run** the executable:

  - ```
    cabal run solver <word-list> <grid-list>
    ```
#### Output

- The Solver uses the Haskell `Maybe` monad to output the results of the search for the provided words.

- The output of the solver will be a list of **tuples** of the following form:

  - If the word was **<u>found</u>**:

    - ```
      (<word>, Just (<position>, <orientation>))
      ```

    - Where:

      - `word` is the word being searched for.
      - `position` is the position of the word within the grid (*position (0,0) is in the top-left*).
      - `orientation` is the orientation of the word at it's position (`Forward`,`Back`, `Up`, `Down`, `UpForward`, `UpBack`, `DownForward`, `DownBack`).

  - If the word was **<u>not found</u>**:

    - ```
      (<word>, Nothing)
      ```

    - Where:

      - `word` is the word being searched for.
      - `Nothing` signifies that the word was not found in the grid.

- <u>*Example*</u>:

  - **Input**:

    - ```
      ./solver bob,jane,tom,frank,ben,bill FKJBOEOEB,ABOIEBENO,NKTNLLTJL,NEREETROB,AFBOFKOEE,BRBOLRRMN,IANOBBAEI,LJANEENNE,LREEFNETK
      ```

      - *i.e.,* the grid produced in the first Generator example. 

  - **Output**:

    - ```
      [("BOB",Just ((2,4),DownForward)),("JANE",Just ((1,7),Forward)),("TOM",Just ((5,3),DownForward)),("FRANK",Just ((4,4),DownForward)),("BEN",Just ((5,1),Forward)),("BILL",Just ((0,5),Down))]
      ```

---

