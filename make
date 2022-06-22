rgbasm -L -o ./out/apollo.o ./src/main.asm && \
rgblink -o ./out/apollo.gb ./out/apollo.o && \
rgblink -n ./out/apollo.sym ./out/apollo.o && \
rgbfix -v -p 0xFF ./out/apollo.gb
