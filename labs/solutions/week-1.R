################################################################################
# Possible solution for lab - R Basics
################################################################################

PATH <- './words.txt'
word_list <- read.delim(PATH, sep = '\n', header = FALSE)$V1

# Function to play the hangman game
play_hangman <- function(word_list = c("apple", "banana", "cat", "dog", "horse")) {
    # Pick a random word from the word list and convert it to list.
    word <- sample(word_list, 1)
    word_split <- unlist(strsplit(word, ''))

    # Initialize the hidden word
    hidden_word <- rep('-', length(word_split))
    
    # Initialize the game state
    lives <- 7
    guessed_letters <- c()
    played_letters <- c()
    # Start the game loop
    while (lives > 0) {
        print(paste("You have", lives, "lives."))
        print(paste("You have played the letters:", played_letters))
        print(hidden_word)
        
        # Prompt the user for a letter
        letter <- readline("Guess a letter: ")

        played_letters <- paste0(c(played_letters, letter), collapse = "")
        # Check if the letter is in the word
        if (letter %in% word_split) {
            # The letter is in the word, update the game state
            print("It's in!")
            guessed_letters <- c(guessed_letters, letter)

            hidden_word[which(letter == word_split)] <- letter

            # Remove the letter from the word
            word_split <- gsub(letter, "", word_split)
            # Check if the user has guessed the entire word
            if (length(unique(word_split)) == 1) {
                # The user has won!
                print("Congratulations, you won!")
                print(paste("The word was:", word))
                break
            }
        } else {
            # The letter is not in the word, decrement the lives
            print(paste("The letter", letter , "is not in the word."))
            lives <- lives - 1
        }

        # The game is over
        if (lives == 0) {
            print("You lost!")
            print(paste("The word was:", word))
        }
    }

}

# Play the game
play_hangman()
