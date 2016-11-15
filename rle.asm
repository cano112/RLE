DATA segment

		BUFFER_SIZE=16384

		ArgV						db 256 dup(?)				;args table
		ArgC						db 0								;args number
		ArgPtr      		dw 9 dup(?)					;pointers to args

		InputPtr				dw ?
		OutputPtr				dw ?
		Mode						db 0								;compressing (0) or decompressing (1)

		InBuffer 					db BUFFER_SIZE dup('$')
		InBufferPtr					dw ?
		BytesInBufferLeftCount  	dw 0

		OutBuffer					db BUFFER_SIZE dup('$')
		OutBufferPtr				dw ?
		BytesInBuffer				dw 0

		InHandler					dw ?							;input handler
		OutHandler					dw ?						;output handler

		EoL							db 10, 13, '$'			;end of line

		SuccessMsg					db "success!$"
		StatusMsg1					db "Opening input file... $"
		StatusMsg2					db "Closing input file... $"
		StatusMsg3					db "Creating output file... $"
		StatusMsg4					db "Opening output file... $"
		StatusMsg5					db "Closing output file... $"
		StatusMsg6					db "Compressing... $"
		StatusMsg7					db "Decompressing... $"

		ErrMsg1						db "Not enough arguments!$"
		ErrMsg2						db "Too many arguments!$"
		ErrMsg3						db "Syntax error!$"
		ErrMsg4						db "Wrong first argument!$"
		ErrMsg5						db "Wrong second argument!$"
		ErrMsg6						db "opening file error!$"
		ErrMsg7						db "closing file error!$"
		ErrMsg8						db "creating file error!$"

DATA ends
CODE segment

	START:

		mov ax, seg STACK1						;init stack
		mov ss, ax
		mov sp, offset StackPointer

		mov ax, seg DATA							;init data segment
		mov ds, ax

		call parseArguments
		call checkArguments
		call openInputFile
		call createOutputFile
		call openOutputFile
		call checkMode
		call closeInputFile
		call closeOutputFile



	PROGRAM_END:
		mov ax, 04c00h
		int 21h


	parseArguments:
		push si
		push dx
		push ax
		push cx
		push bx

		xor cx, cx											;same as "mov cx, 0"
		mov cl, byte ptr es:[80h]				;load args length to cx (loop counter)
		mov si, 82h											;set source pointer
		mov al, es:[si]
		mov di, offset ArgV							;set destination pointer

		cmp cl, 0												;check if ArgC>0
		jne parseArgumentsLoop

		mov dx, offset ErrMsg1					;print error message
		call handleError

		parseArgumentsLoop:							;load args to ArgV
			mov al, byte ptr es:[si]

			cmp al, 20h										;check if space
			je whiteSpace									;if space, skip to white space removal

			cmp al, 9h										;check if tab
			je whiteSpace									;if tab, skip to white space removal

			cmp al, 0Dh										;check if CR (enter pressed)
			je endOfArgs

			mov byte ptr ds:[di], al			;if normal char => load to ArgV
			inc si
			jmp parseArgumentsLoopCont

			endOfArgs:
			mov byte ptr ds:[di], '$'			;replace CR with end of text
			inc si
			jmp parseArgumentsLoopCont

			whiteSpace:										;if white space found
			mov byte ptr ds:[di], 0				;load args separator

			whiteSpaceLoop:								;remove all white spaces left
				inc si
				mov al, byte ptr es:[si]
				cmp al, 20h									;check if space
				je whiteSpaceLoop						;if space found => skip to next char
				cmp al, 9h									;check if tab
				je whiteSpaceLoop						;if tab found => skip to next char

			parseArgumentsLoopCont:		    ;at this point, we do not have any white spaces left
			inc di

		loop parseArgumentsLoop

		mov si, offset ArgV							;load args source pointer
		mov di, offset ArgPtr						;load arg pointers table
		mov bx, offset ArgC							;load arg number pointer

		mov word ptr ds:[di], si				;load first arg pointer
		inc si
		inc byte ptr ds:[bx]
		add di, 2


		separateArgsLoop:
			mov al, byte ptr ds:[si]			;load char to al

			inc si												;point next char
			cmp al, '$'										;if char == $
			je parseArgumentsCont					;break
			cmp al, 0											;if char != NUL
			jne separateArgsLoop					;continue
												;else
			mov word ptr ds:[di], si			;load arg pointer to ArgPtr
			inc byte ptr ds:[bx]					;increment ArgC

			add di, 2											;go to next arg pointer in ArgPtr (word=2byte)
			jmp separateArgsLoop

		parseArgumentsCont:

																		;fix first argument when there are more than one white spaces before first arg (DOSBox issue)
		mov bx, 0
		call getArgument
		cmp byte ptr ds:[bx], 0
		jne fixSpaceCont
			inc word ptr ds:[ArgPtr]

		fixSpaceCont:


		pop bx
		pop cx
		pop ax
		pop dx
		pop si
	ret

	getArgumentLength:								;returns in al length of argument with index from al (0, 1, 2, 3, etc.)
		push si
		push bx


		xor bx, bx											;getArgument uses whole bx
		mov bl, al
		call getArgument
		xor ax, ax											;reset counter

		getArgumentLengthLoop:
			inc al
			inc bx
			cmp byte ptr ds:[bx], '$'			;if end of argument => break
			jne getArgumentLengthLoop

		pop bx
		pop si
	ret

	getArgument:											;returns argument offset in bx (arg number from bx)
		push si

		sal bl, 1d
		mov si, offset ArgPtr
		mov bx, word ptr ds:[si+bx]

		pop si
	ret

	printArgument:										;prints argument with index from al (0, 1, 2, 3, etc.); call parseArguments first
		push si
		push dx
		push bx

		xor bx, bx											;getArgument uses whole bx
		mov bl, al

		call getArgument
		mov dx, bx
		call print

		mov dx, offset EoL							;print EoL
		call print

		pop bx
		pop dx
		pop si
	ret


	checkArguments:
		push bx
		push dx
		push ax
																		;check arg number
		mov bx, offset ArgC
		cmp byte ptr ds:[bx], 2d
		jne checkArgumentsCont1
																		;if ArgC = 2
			push bx

			mov bx, 0
			call getArgument
			mov word ptr ds:[InputPtr], bx
			mov bx, 1
			call getArgument
			mov word ptr ds:[OutputPtr], bx

			pop bx
			jmp checkArgumentsCont3

		checkArgumentsCont1:


		cmp byte ptr ds:[bx], 3d
		je checkArgumentsCont2
			mov dx, offset ErrMsg3				;write out error msg
			call handleError

			checkArgumentsCont2:
			push bx												;if ArgC = 3

				mov bx, 0
				call getArgument
				cmp byte ptr ds:[bx], '-'
				je firstArgCheck1
					mov dx, offset ErrMsg4
					call handleError

				firstArgCheck1:
					cmp byte ptr ds:[bx+1], 'd'
					je firstArgCheck2
						mov dx, offset ErrMsg4
						call handleError
					firstArgCheck2:



					mov bx, 1
					call getArgument
					mov word ptr ds:[InputPtr], bx
					mov bx, 2
					call getArgument
					mov word ptr ds:[OutputPtr], bx
					mov byte ptr ds:[Mode], 1

			pop bx

		checkArgumentsCont3:

		pop ax
		pop dx
		pop bx
	ret

	openInputFile:
		push ax
		push dx

		mov dx, offset StatusMsg1
		call print

		mov dx, word ptr ds:[InputPtr]			;filename
		mov ah, 03dh												;open file
		xor al, al													;read mode
		int 21h

		jnc openInputFileSuccess
			mov dx, offset ErrMsg6
			call handleError

		openInputFileSuccess:
			mov word ptr ds:[InHandler], ax
			mov dx, offset SuccessMsg
			call print

			mov dx, offset EoL
			call print

		pop dx
		pop ax
	ret

	closeInputFile:
		push ax
		push bx
		push dx

		mov dx, offset StatusMsg2
		call print

		mov bx, word ptr ds:[InHandler]				;file handler
		mov	ah, 03eh													;close file
		int	21h
		jnc closeInputFileSuccess
			mov dx, offset ErrMsg7
			call handleError

		closeInputFileSuccess:
			mov dx, offset SuccessMsg
			call print

			mov dx, offset EoL
			call print

		pop dx
		pop bx
		pop ax
	ret

	createOutputFile:
		push ax
		push dx
		push cx

		mov dx, offset StatusMsg3
		call print

		mov dx, word ptr ds:[OutputPtr]
		mov ah, 03ch													;create file
		mov cx, 10100000b											;file attributes: shareable+archive
		int 21h

		jnc createOutputFileSuccess
			mov dx, offset ErrMsg8
			call handleError

		createOutputFileSuccess:
			mov dx, offset SuccessMsg
			call print

			mov dx, offset EoL
			call print

		pop cx
		pop dx
		pop ax
	ret

	openOutputFile:
		push ax
		push dx

		mov dx, offset StatusMsg4
		call print

		mov dx, word ptr ds:[OutputPtr]				;filename
		mov ah, 03dh													;open file
		mov al, 1d														;write mode
		int 21h

		jnc openOutputFileSuccess
			mov dx, offset ErrMsg6
			call handleError

		openOutputFileSuccess:
			mov word ptr ds:[OutHandler], ax
			mov word ptr ds:[OutBufferPtr], offset OutBuffer

			mov dx, offset SuccessMsg
			call print

			mov dx, offset EoL
			call print

		pop dx
		pop ax
	ret

	closeOutputFile:
		push ax
		push bx
		push dx

		mov dx, offset StatusMsg5
		call print

		mov bx, word ptr ds:[OutHandler]			;file handler
		mov	ah, 03eh													;close file
		int	21h
		jnc closeOutputFileSuccess
			mov dx, offset ErrMsg7
			call handleError

		closeOutputFileSuccess:
			mov dx, offset SuccessMsg
			call print

			mov dx, offset EoL
			call print

		pop dx
		pop bx
		pop ax
	ret

	getChar:																;char loaded to ah, if al==1 then EoF
		push bx

		xor al, al
		cmp word ptr ds:[BytesInBufferLeftCount], 0
		jne skipGettingFromFile
			call loadToBuffer
			cmp word ptr ds:[BytesInBufferLeftCount], 0
			je endOfFile


		skipGettingFromFile:
		mov bx, word ptr ds:[InBufferPtr]
			mov ah, byte ptr ds:[bx]
			inc word ptr ds:[InBufferPtr]
			dec word ptr ds:[BytesInBufferLeftCount]
			jmp skipEoF

		endOfFile:
			mov al, 1

		skipEoF:

		pop bx
	ret

	putChar:																;char loaded from ah
		push bx
			mov bx, word ptr ds:[OutBufferPtr]
			mov byte ptr ds:[bx], ah
			inc word ptr ds:[BytesInBuffer]
			inc word ptr ds:[OutBufferPtr]

		cmp word ptr ds:[BytesInBuffer], BUFFER_SIZE
		jne skipLoadingToFile
			call loadToFile

		skipLoadingToFile:

		pop bx
	ret

	loadToBuffer:														;returns in BytesInBufferLeftCount number of bytes actually read
		push dx
		push bx
		push cx
		push ax

		mov word ptr ds:[InBufferPtr], offset InBuffer
		mov dx, offset InBuffer
		mov bx, word ptr ds:[InHandler]
		mov cx, BUFFER_SIZE
		mov ah, 03fh													;read from ds:bx to ds:dx
		int 21h

		mov word ptr ds:[BytesInBufferLeftCount], ax

		pop ax
		pop cx
		pop bx
		pop dx
	ret

	loadToFile:
		push bx
		push cx
		push ax
		push dx

		mov word ptr ds:[OutBufferPtr], offset OutBuffer
		mov bx, word ptr ds:[OutHandler]
		mov dx, offset OutBuffer
		mov cx, word ptr ds:[BytesInBuffer]
		mov ah, 040h
		int 21h

		mov word ptr ds:[BytesInBuffer], 0

		pop dx
		pop ax
		pop cx
		pop bx
	ret


	compress:
		push dx
		push ax
		push bx
		push cx

		mov dx, offset StatusMsg6
		call print

		call getChar
		cmp al, 0															;check if not EoF
		jne compressLoopBreak
		mov bh, ah
		mov ch, 1

		compressLoop:
			call getChar
			cmp al, 0														;check if not EoF
			jne compressLoopBreak

			cmp ah, bh													;ah - actual char, bh - previous char
			jne serieBreak
				cmp ch, 255												;break if serie>255
				je serieBreak

				inc ch
				jmp compressLoop

			serieBreak:
				push ax
				cmp bh, 0													;if nullbyte
				jne skipNullByteLoopL
					mov ah, bh
					nullByteLoopL:
						call putChar									;replace with double nullbyte
						call putChar
						dec ch
						cmp ch, 0
					jne nullByteLoopL

					jmp skipDoNotModifyL

				skipNullByteLoopL:

				cmp ch, 3												;if serie<=3 then do not modify
				jbe doNotModifyL								;jump below or equal

				mov ah, 0												;else modify
				call putChar
				mov ah, ch
				call putChar
				mov ah, bh
				call putChar
				jmp skipDoNotModifyL

				doNotModifyL:
				mov ah, bh											;if we do not modify, we have to put char the same number of times we've read it from file
				doNotModifyLoopL:
					call putChar
					dec ch
					cmp ch, 0
				jne doNotModifyLoopL

				skipDoNotModifyL:
				pop ax

				mov bh, ah
				mov ch, 1

		jmp compressLoop

		compressLoopBreak:									;after exiting the loop, we have to put last serie to buffer

		push ax
		cmp bh, 0
		jne skipNullByteLoop
			mov ah, bh
			nullByteLoop:
				call putChar
				call putChar
				dec ch
				cmp ch, 0
			jne nullByteLoop

			jmp skipDoNotModify

		skipNullByteLoop:

		cmp ch, 3
		jbe doNotModify											;jump below or equal

		mov ah, 0
		call putChar
		mov ah, ch
		call putChar
		mov ah, bh
		call putChar
		pop ax
		jmp skipDoNotModify

		doNotModify:
		mov ah, bh
		doNotModifyLoop:
			call putChar
			dec ch
			cmp ch, 0
		jne doNotModifyLoop

		skipDoNotModify:
		pop ax

		call loadToFile

		mov dx, offset SuccessMsg
		call print

		mov dx, offset EoL
		call print

		pop cx
		pop bx
		pop ax
		pop dx
	ret

	decompress:
		push dx
		push ax
		push cx
		push bx

		mov dx, offset StatusMsg7
		call print

		decompressLoop:
			call getChar
			cmp al, 0													;if EoF
			jne decompressLoopBreak

			cmp ah, 0													;0 means modified or null
			je nullByteOrModified
				call putChar
				jmp skipNullByteOrModified

			nullByteOrModified:
				call getChar
				cmp al, 0												;if eof
				jne decompressLoopBreak

				cmp ah, 0
				je nullByte											;that means nullbyte
					mov ch, ah

					call getChar
					cmp al, 0
					jne decompressLoopBreak

					modifiedLoop:
						call putChar								;if modified
						dec ch
						cmp ch, 0
					jne modifiedLoop

					jmp skipNullByte

				nullByte:
					call putChar

				skipNullByte:

			skipNullByteOrModified:
		jmp decompressLoop

		decompressLoopBreak:

		call loadToFile

		mov dx, offset SuccessMsg
		call print

		mov dx, offset EoL
		call print

		pop bx
		pop cx
		pop ax
		pop dx
	ret

	checkMode:
		cmp byte ptr ds:[Mode], 0
		je compressing
			call decompress
			jmp skipCompressing
		compressing:
			call compress

		skipCompressing:

	ret

	print:																;prints text from ds:dx, use "mov dx, offset <label>" before
		push ax

		mov ah, 9h
		int 21h

		pop ax
	ret

	handleError:
		push ax

		call print
		mov ax, 04c00h
		int 21h

		pop ax
	ret



CODE ends

STACK1 segment STACK
							dw 127 dup(?)
		StackPointer		dw ?

STACK1 ends

end START
