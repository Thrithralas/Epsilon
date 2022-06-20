builtin operator infix 4 <(int,int) : bool
builtin operator infixl 6 +(int, int) : int
builtin operator infixl 6 -(int, int) : int
builtin operator infixl 7 *(int, int) : int
builtin operator infixl 7 /(int, int) : int
builtin operator infixr 8 ^(int, int) : int
builtin operator infix 4 ==(int, int) : bool
builtin operator infixr 3 &&(bool,bool) : bool
builtin operator infixr 2 ||(bool,bool) : bool

builtin function print(int) : void
builtin function printStr(string) : void

function abs(a : int) : int
{
  if a < 0 { return (0 - a) }
  return a
}

operator infixl 7 %(a : int, b : int) : int
{
  i := a
  j := abs(b)
  while (i < 0)
  {
    i := i + j
  }
  while (j < i)
  {
    i := i - j
  }
  return i
}

function isEven(a : int) : bool
{
  i := abs(a)
  if (i == 0) { return true }
  else { if (i == 1) { return false } else { return isOdd(i - 1) } }
}

function isOdd(alma : int) : bool
{
  x := abs(alma)
  if (i == 0) { return false }
  else { if (i == 1) { return true } else { return isEven(i - 1) } }
}

function not(b : bool) : bool
{
  if b { return false }
  return true
}

function chi(b : bool) : int
{
  if b { return 1 }
  else { return 0 }
}

function isPrime(a : int) : bool
{
  if isEven(a) { return false }
  b := 1
  while (b * b < a || b * b == a)
  {
    b := b + 1
  }
  b := b - 1
  i := 3
  res := true
  while (res && i < b)
  {
    if(a % i == 0)
    {
      res := false
    }
    i := i + 2
  }
  return res
}

print(chi(isPrime(123)))
print(chi(isPrime(23)))
print(chi(isPrime(27)))
print(chi(isPrime(100)))
print(chi(isPrime(1009)))