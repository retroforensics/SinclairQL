Yes, yet another JS rom disassembly.

I did this one in a form I wanted, not as a collection of fragmented files.
That way I could quickly search for and find the information I wanted.

The comments are all my own, perhaps only in a sense that only I understand.
Far from complete, something I may or may not get around to adding to, or
eventually complete one day.

You can reassemble the files to a identical copy of the original JS rom using
Quanta's Assembler Workbench. Just put the files in ram1_ (or elsewhere).

  ASM 0 flp1_          load assembler from flp1_ (dummy address 0)
  <enter>              return to Cmd> prompt
  DEV ram1_            define default device for input (includes/library)
  ASM                  return to assembler Asm> prompt
  OUT ram1_myrom       binary output filename
  IN jsrom_asm         root asm file

after a while (perhaps long while) you should get a copy of JS rom.

(Minerva source could benefit from being done similarly...)
