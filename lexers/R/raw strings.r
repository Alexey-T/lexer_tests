# ==============================================================================
# R 4.0+ Raw String Literals - Comprehensive Examples
# ==============================================================================

# ------------------------------------------------------------------------------
# 2. Basic Syntax Variations (lowercase 'r' and uppercase 'R')
# ------------------------------------------------------------------------------

# Lowercase 'r' with double quotes as delimiter starters
basic_lower <- r"(Hello World)"

# Uppercase 'R' with double quotes as delimiter starters
basic_upper <- R"(Hello World)"

# Using single quotes as delimiter starters
basic_single <- r'[Hello World]'

# ------------------------------------------------------------------------------
# 3. Handling Special Characters (No Escaping Needed)
# ------------------------------------------------------------------------------

# Windows file paths (backslashes do not need escaping)
windows_path <- r"(C:\Users\Name\Documents\file.txt)"

# Literal backslash-n (not interpreted as newline)
literal_escape <- r"(This is \n not a newline)"

# Regular expressions (no double escaping required)
regex_pattern <- r"(\d+\.\d+)"

# ------------------------------------------------------------------------------
# 4. Handling Quotes Inside Strings
# ------------------------------------------------------------------------------

# Including double quotes inside (using square brackets as delimiters)
contains_double_quotes <- r"[He said "Hello" to me]"

# Including single quotes inside (using parentheses as delimiters)
contains_single_quotes <- r"(It's a nice day)"

# Including both quotes (using curly braces as delimiters)
contains_both_quotes <- r"{She said 'Hi' and "Bye"}"

# ------------------------------------------------------------------------------
# 5. Multiline Raw Strings
# ------------------------------------------------------------------------------

multiline_string <- r"(
Line 1 of text
Line 2 of text
Line 3 of text
)"

# ------------------------------------------------------------------------------
# 6. Custom Delimiters (for nested content)
# ------------------------------------------------------------------------------

# Using hyphens to allow closing brackets inside the string
custom_delim_1 <- r"--[Text with ] inside]--"

# Using equals signs to allow curly braces inside
custom_delim_2 <- r"={Text with } inside}="

# ------------------------------------------------------------------------------
# 7. Comparison: Normal Strings vs. Raw Strings
# ------------------------------------------------------------------------------

# Normal string (requires escaping backslashes and quotes)
normal_string <- "C:\\Users\\Name\\file.txt"
normal_regex  <- "\\d+\\.\\d+"

# Raw string (cleaner syntax)
raw_string    <- r"(C:\Users\Name\file.txt)"
raw_regex     <- r"(\d+\.\d+)"
