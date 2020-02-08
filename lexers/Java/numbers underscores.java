long creditCardNumber = 1234_5678_9012_3456L;
long socialSecurityNumber = 999_99_9999L;
float pi =      3.14_15F;
long hexBytes = 0xFF_EC_DE_5E;
long hexWords = 0xCAFE_BABE;
long maxLong = 0x7fff_ffff_ffff_ffffL;
byte nybbles = 0b0010_0101;
long bytes = 0b11010010_01101001_10010100_10010010;

float pi1 = 3_.1415F;      // Invalid; cannot put underscores adjacent to a decimal point
float pi2 = 3._1415F;      // Invalid; cannot put underscores adjacent to a decimal point
long socialSecurityNumber1
  = 999_99_9999_L;         // Invalid; cannot put underscores prior to an L suffix

int x1 = _52;              // This is an identifier, not a numeric literal
int x2 = 5_2;              // OK (decimal literal)
int x3 = 52_;              // Invalid; cannot put underscores at the end of a literal
int x4 = 5_______2;        // OK (decimal literal)

int x5 = 0_x52;            // Invalid; cannot put underscores in the 0x radix prefix
int x6 = 0x_52;            // Invalid; cannot put underscores at the beginning of a number
int x7 = 0x5_2;            // OK (hexadecimal literal)
int x8 = 0x52_;            // Invalid; cannot put underscores at the end of a number

int x9 = 0_52;             // OK (octal literal)
int x10 = 05_2;            // OK (octal literal)
int x11 = 052_;            // Invalid; cannot put underscores at the end of a number

