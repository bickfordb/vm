all: vm

test: vm
	@echo "1 + 1 ="
	@./vm < bytecode/add1.code
	@echo "2 * 2 ="
	@./vm < bytecode/mul2.code
	@echo "None * None =>"
	@./vm < bytecode/null.code
vm: VM.hs
	ghc -XScopedTypeVariables -XTypeSynonymInstances --make VM.hs -o vm

clean:
	-rm -rf VM.c VM.hi VM.o vm VM


