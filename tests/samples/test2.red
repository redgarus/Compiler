func fib[n] {
    if[n <= 1] do
        return n;
    return fib[n-1] + fib[n-2];
}

print[fib[10]];