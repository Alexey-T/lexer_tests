<?

class Test {
    private ?Traversable $traversable = null;
    private ?Countable $countable = null;
    /** @var Traversable&Countable */
    private $both = null;
 
    public function __construct($countableIterator) {
        $this->traversable =& $this->both;
        $this->countable =& $this->both;
        $this->both = $countableIterator;
    }
}

enum Suit: string {
  case Hearts = 'H';
  case Diamonds = 'D';
  case Clubs = 'C';
  case Spades = 'S';
}
