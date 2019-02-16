main()
{
    int i,freq, bits, bits2, freq2;
    int freq2bits();bits2freq();

    for (i=1;i<=0xFFFFF;i++) {
        bits=freq2bits(i);
        bits2=freq2bitsx(i);
        freq=bits2freq(bits);
        freq2=bits2freq(bits2);
      if(bits != bits2 || (i%0x10000 == 0))
        printf(" i %ld bits %lx freq %ld % bits2 %lx freq2 %ld\n",i,bits,freq,
            bits2, freq2);
    }
}
int freq2bits(freq)
int freq;                    /* frequency in 10's of KHz, 50000-99999 legal */
{
    return( 0xFFFFF & ((~freq)+0x10) );
}
int freq2bitsx(freq)
int freq;                    /* frequency in 10's of KHz, 50000-99999 legal */
{
    int bits;

/*  return( 0xFFFFF & ((~freq)+0x10) );*/
    bits= ((0xFFFFF &~freq)+0x10);
    return (bits);

}
int bits2freq(bits)
unsigned int bits;
{
     return( 0xFFFFF & ~(bits-0x10));
}
