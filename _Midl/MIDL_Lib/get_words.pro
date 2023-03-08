Function Get_words, inp, full = ful, show = sho

;+
; NAME:
;		GET_WORDS
; VERSION:
;		8.72
; PURPOSE:
;		Finds words matching an input word with missing letters.
; CATEGORY:
;		Utility.
; CALLING SEQUENCE:
;		Result = GET_WORDS (INP [keywords])
; INPUTS:
;	INP
;		Scalar string, representing single word. Some letters may be missing,
;		replaced by the wildcard '*'.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/FULL
; 		Switch.  If set, a larger word list (containing about 370000 words) is
; 		used for comparison.  By default, a shorter list (containing about 60000
; 		words) is used.
; 	/SHOW
; 		Switch.  If set, the result being returned is also printed to the
; 		screen.
; OUTPUTS:
;		Returns a list of all the words in the wordfile matching INP.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		WORDLIST.  Contains
;			EXS		-	Flag.  Set to 1 once the worldfiles hav been read.
;			WORDS1	-	Short word array, ~ 60000 words.
;			WORDS2	-	Long word array, ~ 370000 words.
;			WLENS1	-	Array containing the lengths of all the words in WORDS1.
;			WLENS2	-	Array containing the lengths of all the words in WORDS2.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The input INP must be a character string.
; PROCEDURE:
;		Straightforward, using wordlists downloaded automatically from the save
;		file WORDLISTS located in MIDL\_Midl\Data.  Calls TYPE from MIDL and
;		SDEP from IMPORTS.
; MODIFICATION HISTORY:
;		Created 25-JAN-2021 by Mati Meron.
;-

	common wordlist, exs, words1, words2, wlens1, wlens2

	on_error, 1

	if Type(exs) eq 0 then begin
		ds = Sdep(/ds)
		savfile= getenv('root_mm')+ ds+ '_midl'+ ds+ 'data'+ ds+ 'wordlists.sav'
		restore, file = savfile
		wlens1 = strlen(words1)
		wlens2 = strlen(words2)
		exs = 1
	endif

	if keyword_set(ful) then begin
		words = words2
		wlens = wlens2
	endif else begin
		words = words1
		wlens = wlens1
	endelse

	wild = '*'
	bwild = (byte(wild))[0]
	res = []
	if Type(inp) eq 7 then begin
		len = Strlen(inp)
		winp = strlowcase(inp)
		fir = where(wlens eq len,nfir)
		if nfir gt 0 then begin
			comp = words[fir]
			sub = where(byte(inp) eq bwild,nsub)
			if nsub gt 0 then begin
				barr = byte(comp)
				barr[sub,*] = bwild
				comp = string(barr)
			endif
			sec = where(comp eq inp,nsec)
			if nsec gt 0 then res = words[fir[sec]]			
		endif
	endif else message, 'Character string input needed!

	if keyword_set(sho) then begin
		if n_elements(res) gt 0 then print, res, form = '(a)' else print, res
	endif

	return, res
end