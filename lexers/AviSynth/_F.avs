Function NTSC2PAL2( clip c) {
    Assert(c.height == 480, "NTSC2PAL: ������� ���� ������ ����� ���������� � 480 �����")
    Bob(c, height=576)
    return Weave()
}


Function NTSC2PAL( clip c) {
    Assert(c.height == 480, "NTSC2PAL: ������� ���� ������ ����� ���������� � 480 �����")
    Bob(c, height=576)
    return Weave()
}
