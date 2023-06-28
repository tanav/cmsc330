/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 { return -1; }

    let mut i = 0;
    let mut result = 0;

    while i != n {
        result += n - i;
        i += 1;
    }
    
    result
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut counter = 0;
    
    for x in ls {
      if x >= &s && x <= &e {
        counter += 1;
      }
    }
    
    counter
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    let mut counter = 0;
    
    for x in target {
        for y in set {
            if x == y {
                counter += 1;
            }
        }
    }
    
    counter == target.len()
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.len() == 0 { return None; }

    let mut counter = 0.0;
    for x in ls {
        counter += x;
    }

    Some(counter / ls.len() as f64)
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut result = 0;
    let mut counter = 1;

    for x in ls.iter().rev() {
          match x {
              &1 => {result += counter; counter *= 2;}
              _ => {counter *= 2;}
          }
      }
  
    result
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut num = n.clone();
    let mut result = Vec::new();
    
    let mut divisor = 2;
    while num != 1 {
        while divisor <= num && num % divisor != 0 {
            divisor += 1;
        }
        
        num /= divisor;
        result.push(divisor);
    }  
    if num > 1 { result.push(num); }
    
    result
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut result = Vec::new();

    if lst.len() == 0 { return Vec::new(); }

    if lst.len() < 2 {
        for x in lst {
            result.push(*x);
        }
        
        return result;
    }

    let mut i = 1;
    while i < lst.len() {
        result.push(lst[i]);
        i += 1;
    }
    result.push(lst[0]);
    
    result
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if s.as_str() == target { return true; }
    
    let mut string = s.clone();
    let length = s.len();
    
    let mut index = 0;
    while length > index {
        if string.starts_with(target) {
            return true;
        }
        
        if string.len() == 0 { 
            break;
        }
        
        string = string[1..].to_string();
        index += 1;
    }
    
    false
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
	if s == "" { return None; }

    let mut target = s.chars().nth(0).unwrap();
    let mut start = 0;
    let mut length = 1;
    
    let mut index = 0;
    let mut counter = 0;

    for x in s.chars() {
        if target != x {
            if length < counter {
                start = index - counter;
                length = counter;
            }
            
            target = x;
            counter = 0;
        }

        index += 1;
        counter += 1;
    }

    Some(&(&s[start..start+length]))
}