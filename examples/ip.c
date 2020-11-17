float ipf (float x[],
           float y[],
           int n) {
    int i;
    float result = 0.0;

    for (i = 0; i < n; i++)
        result += x[i] * y[i];
    return result;
}
