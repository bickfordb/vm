
test: VM
	./VM

VM: VM.hs
	ghc --make VM

clean:
	-rm -rf VM.hi VM.o VM

