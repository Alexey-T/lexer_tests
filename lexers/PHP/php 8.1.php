<?
enum Suit {
  case Hearts;
  case Diamonds;
  case Clubs;
  case Spades;
}

$a instanceof Suit;  // true

interface Colorful {
  public function color(): string;
}
 
enum Suit implements Colorful {
  case Hearts;
  case Diamonds;
  case Clubs;
  case Spades;
 
  // Fulfills the interface contract.
  public function color(): string {
    return match($this) {
      Suit::Hearts, Suit::Diamonds => 'Red',
      Suit::Clubs, Suit::Spades => 'Black',
    };
  }
 
  // Not part of an interface; that's fine.
  public function shape(): string {
    return "Rectangle";
  }
}
 
enum Size {
  case Small;
  case Medium;
  case Large;
 
  public static function fromLength(int $cm) {
    return match(true) {
      $cm < 50 => static::Small,
      $cm < 100 => static::Medium,
      default => static::Large,
    };
  }
}

0o16 === 14; // true
0o123 === 83; // true
 
0O16 === 14; // true
0O123 === 83; // true
 
016 === 0o16; // true
016 === 0O16; // true

function redirect(string $uri): noreturn {
    header('Location: ' . $uri);
    exit();
}
 
function redirectToLoginPage(): noreturn {
    redirect('/login');
}