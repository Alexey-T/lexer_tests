import std::io, std::math::random;

enum Action : (String abbrev, String full)
{
    ROCK = { "r", "Rock" },
    PAPER = { "p", "Paper" },
    SCISSORS = { "s", "Scissors" },
}

const ROUNDS = 3;

fn int main()
{
    int p_score;
    int c_score;
    int rounds = ROUNDS;

    io::printfn("Let's play Rock-Paper-Scissors!");
    while (rounds > 0)
    {
        io::printfn("Best out of %d, %d rounds remaining. ", ROUNDS, rounds);
        io::printn("What is your guess? [r]ock, [p]aper, or [s]cissors?");

        Action guess;
        while (true)
        {
            String? s = io::treadline();
            if (catch s) return 1;

            if (try current_guess = Action.lookup_field(abbrev, s))
            {
                guess = current_guess;
                break;
            }
            io::printn("input invalid.");
        }

        io::printfn("Player: %s", guess.full);

        Action comp = Action.from_ordinal(rand(3));
        io::printfn("Computer: %s", comp.full);

        switch
        {
            case comp == ROCK && guess == SCISSORS:
            case comp == SCISSORS && guess == PAPER:
            case comp == PAPER && guess == ROCK:
                 io::printn("Computer Score!");
                 c_score++;
                 rounds--;
            case guess == ROCK && comp == SCISSORS:
            case guess == SCISSORS && comp == PAPER:
            case guess == PAPER && comp == ROCK:
                io::printn("Player Score!");
                p_score++;
                rounds--;
            default:
                io::printn("Tie!");
        }
        io::printfn("Score: Player: %d, Computer: %d", p_score, c_score);
    }

    switch
    {
        case p_score < c_score: io::printn("COMPUTER WINS GAME!");
        case p_score > c_score: io::printn("PLAYER WINS GAME!");
        default:                io::printn("GAME TIED!");
    }
    io::printn("Thank you for playing.");
    return 0;
}