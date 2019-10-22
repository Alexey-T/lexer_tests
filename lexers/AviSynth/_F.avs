Function NTSC2PAL2( clip c) {
    Assert(c.height == 480, "NTSC2PAL: входной клип должен иметь разрешение в 480 строк")
    Bob(c, height=576)
    return Weave()
}


Function NTSC2PAL( clip c) {
    Assert(c.height == 480, "NTSC2PAL: входной клип должен иметь разрешение в 480 строк")
    Bob(c, height=576)
    return Weave()
}
