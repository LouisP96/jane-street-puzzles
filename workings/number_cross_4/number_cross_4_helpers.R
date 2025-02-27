library(foreach)
library(doParallel)

is_palindrome <- function(number) {
  # Convert the number to a character string
  num_str <- as.character(number)
  
  # Define a helper function to check if a string is a palindrome
  is_palindrome_helper <- function(str) {
    if (nchar(str) <= 1) {
      return(TRUE)  # Base case: Single character or empty string is a palindrome
    } else {
      first_char <- substr(str, 1, 1)  # Get the first character
      last_char <- substr(str, nchar(str), nchar(str))  # Get the last character
      
      # Check if the first and last characters are equal
      if (first_char != last_char) {
        return(FALSE)  # If not equal, not a palindrome
      } else {
        # Recursively check the substring without the first and last characters
        return(is_palindrome_helper(substr(str, 2, nchar(str) - 1)))
      }
    }
  }
  
  # Call the helper function with the number converted to a string
  return(is_palindrome_helper(num_str))
}

# Palindrome multiples of 23
for (i in 1:100) {
  num <- 23 * i
  if (is_palindrome(num)) {
    print(num)
  }
}

# Generate palindrome multiples of 23 with length 7 and satisfying additional conditions
palindrome_multiples <- numeric()
for (i in 1000000:9999999) {
  if (i %% 23 == 0 && is_palindrome(i)) {
    digits <- as.numeric(strsplit(as.character(i), "")[[1]])
    if (digits[3] != digits[4] && digits[1] == digits[2] && digits[6] == digits[7] &&
        digits[1] == 7 && digits[4] == 4) {
      palindrome_multiples <- c(palindrome_multiples, i)
    }
  }
}
print(palindrome_multiples)


# Square nums
for (i in 1:34) {
  num <- i**2
  print(num)
}

# Square number with last 3 same digits
all_except_first_same <- function(n) {
  digits <- strsplit(as.character(n), "")[[1]]
  if (length(digits) > 3 && length(digits) <= 6) {
    if (digits[length(digits)] == digits[length(digits) - 1] && digits[length(digits) - 1] == digits[length(digits) - 2] 
        # digits[length(digits) - 3] == digits[length(digits) - 4]# && digits[5] == digits[6] &&
        # digits[7] == digits[8] &&
        # 1 == 1
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
    return(FALSE)
}
for (i in 1:316228) {
  num <- i**2
  if (all_except_first_same(num)) {
    print(num)
  }
}

# Square number with first 3 digits the same, (and 4th and 5th the same)
check_criteria <- function(n) {
  if (n >= 100 && n <= 999999) {  # Check if the number is 4+ digits
    digits <- strsplit(as.character(n), "")[[1]]
    if (digits[1] == digits[2] && digits[2] == digits[3] 
        && digits[4] == digits[5]
        ) {
      return(TRUE)
    }
  }
  return(FALSE)
}
for (i in 1:10000) {
  num <- i**2
  if (check_criteria(num)) {
    print(num)
  }
}

# Multiple of 88
for (i in 1:100) {
  num <- i*88
  print(num)
}

# Multiple of 88 with condition
for (i in 1:10000) {
  num <- i*88
  digits <- strsplit(as.character(num), "")[[1]]
  if (
    # digits[length(digits) - 4] == 7 &&
      # digits[length(digits) - 3] == 9 && digits[length(digits) - 2] == 9 && digits[length(digits) - 1] == 9 &&
      # digits[length(digits) - 5] == digits[length(digits) - 6] && digits[length(digits) - 6] == digits[length(digits) - 7] &&
      digits[length(digits)] == digits[length(digits) - 1] && digits[length(digits) - 2] == 1 && digits[length(digits) - 3] == 1 &&
      # length(digits) < 6 &&
      1 == 1
      ) {
      print(num)
  }
}

for (i in 1:100) {
  num <- i*88
  digits <- strsplit(as.character(num), "")[[1]]
  # if (
  #   digits[length(digits) - 0] == 9
  #     ) {
      print(num)
  }
}

# Multiple of 37
for (i in 1:10) {
  num <- i*37
  print(num)
}

# Multiple of 37 ending in 444
ends_in_222 <- function(n) {
  n %% 1000 == 444
}
for (i in 1:10000) {
  num <- i*37
  if (ends_in_222(num)) {
    print(num)
  }
}

# Multiple of 37 with same last 3 digits
last_three_digits_same <- function(n) {
  digits <- strsplit(as.character(n), "")[[1]]
  if (length(digits) == 5 ) {
    if (digits[length(digits)] == digits[length(digits) - 1] && digits[length(digits) - 1] == digits[length(digits) - 2]) {
      return(TRUE)
    }
  }
  return(FALSE)
}
for (i in 1:10000000) {
  num <- i*37
  if (last_three_digits_same(num)) {
    print(num)
  }
}

# Primes
sieve <- function(n)
{
  n <- as.integer(n)
  if(n > 1e8) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}

# Prime to prime powers
num_cores <- 16
cl <- makeCluster(num_cores)
registerDoParallel(cl)

calculate_powers <- function(number, numbers) {
  results <- list()
  for (exponent in numbers) {
    result <- number ^ exponent
    if (result < 1e11) {
      results <- c(results, result)
    }
  }
  return(results)
}

numbers <- sieve(316228)
results <- foreach(number = numbers) %dopar% {
  calculate_powers(number, numbers)
}
out <- sort(unlist(results))
out
stopCluster(cl)

# Check condition
condition <- function(n) {
  digits <- strsplit(as.character(n), "")[[1]]
  if (length(digits) == 5 #&& length(digits) > 1
      ) {
    if (
      # digits[1] != digits[2] && 
      digits[3] == 4 && digits[4] == 4 && digits[5] == 4
        ) {
    return(n)
    }
  }
}
unlist(lapply(out, condition))
