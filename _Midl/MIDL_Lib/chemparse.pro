Pro Chemparse, str, elements = ele, numbers = num, weights = wei, nosort = nos

	;+
	; NAME:
	;		CHEMPARSE
	; VERSION:
	;		8.41
	; PURPOSE:
	;		Parses a chemical formula
	; CATEGORY:
	;		Chemistry utility.
	; CALLING SEQUENCE:
	;		CHEMPARSE, STR, ELEMENTS = ELE, NUMBERS = NUM, WEIGHTS = WEI, $
	;			NOSORT= NOS
	; INPUTS:
	;	STR
	;		Character string, representing a chemical formula.  Mandatory.
	;		The following rules apply:
	;			1)	The formula may include upper and lower case characters,
	;				numbers and opening and closing parentheses.  Any other 
	;				characters will cause errors.
	;			2)	Element symbols *must* start with upper case letter.  Second
	;				letter, when needed, *must* be lower case.
	;			3)	Avoid confusing upper case 'O' with '0'.
	; OPTIONAL INPUT PARAMETERS:
	;		None.
	; KEYWORD PARAMETERS:
	;	ELEMENTS
	;		Optional output, see below.
	;	NUMBERS
	;		Optional output, see below.
	;	WEIGHTS
	;		Optional output, see below.
	;	NOSORT
	;		Switch.  If set, the outputs are not sorted and consolidated.
	; OUTPUTS:
	;		See OPTIONAL OUTPUTS.
	; OPTIONAL OUTPUT PARAMETERS:
	;	ELEMENTS
	;		Character array, contains the elemental symbols present in the 
	;		formula.
	;	NUMBERS
	;		Integer array, contains the numbers of atoms for each element in
	;		ELEMENTS, in same order.
	;	WEIGHTS
	;		Floating array, contains the weights (i.e. mass fractions) of the
	;		elements in the formula.
	;
	;		Note:	Using nonexistent element symbol will cause an error, when
	;				WEIGHTS is invoked.
	; COMMON BLOCKS:
	;		SXR_STUFF.  See LOAD_ABS_COEFFS for more information.
	; SIDE EFFECTS:
	;		None.
	; RESTRICTIONS:
	;		None, other than those listed for STR.
	; PROCEDURE:
	;		Straightforward.  CHEMPARSE calls itself recursively when needed.
	;		Also calls ELECOMP from SRUFF_LIB and BRAKET_CHECK, SORPURGE and
	;		STREQ, from MIDL.
	; MODIFICATION HISTORY:
	;		Created 5-MAR-2015 by Mati Meron.
	;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	len = strlen(str)
	byt = byte(str)
	wele = (wnum = [])

	repeat begin
		nbr = Braket_check(string(byt),bra='(',ket=')',open=op,close=cl)
		if nbr eq 0 then break
		keep = replicate(1,len)
		cl = cl[0]
		op = op[0]
		if cl eq (op+1) or cl eq len-1 then begin
			keep[op] = (keep[cl] = 0)
		endif else begin
			j = (where(byt[cl+1:*] gt 57 or byt[cl+1:*] lt 48))[0]
			if j eq -1 then j = len-cl-1
			if j gt 0 then begin
				mult = fix(string(byt[cl+1:cl+j]))
				pstr = string(byt[op+1:cl-1])
				Chemparse, pstr, ele= pele, num= pnum, /nosort
				wele = [wele,pele]
				wnum = [wnum,mult*pnum]
				keep[op:cl+j] = 0
			endif else keep[op] = (keep[cl] = 0)
		endelse
		lef = where(keep,len)
		if len gt 0 then byt = byt[lef] else nbr = 0
	endrep until nbr eq 0

	while len gt 0 do begin
		i = 0
		if byt[i] ge 65 and byt[i] le 90 then begin
			i = i + 1
			if len gt i then begin
				if byt[i] ge 97 and byt[i] le 122 then i = i + 1
			endif
			ecur = byt[0:i-1]
			wele = [wele,string(ecur)]
			if len gt i then begin
				j = (where(byt[i:*] gt 57))[0]
				if j eq -1 then j = len-i
			endif else j = 0
			if j gt 0 then ncur = byt[i:i+j-1] else ncur = 49b
			wnum = [wnum,fix(string(ncur))]
			len = len - i - j
			if len gt 0 then byt = byt[i+j:*]
		endif else message, 'Bad element entry!'
	endwhile

	if keyword_set(nos) then begin
		ele = wele
		num = wnum
	endif else begin
		ele = wele[Sorpurge(wele,net=n)]
		num = intarr(n)
		for i = 0, n-1 do begin
			dum = where(Streq(wele,ele[i]))
			num[i] = total(wnum[dum],/pres)
		endfor
	endelse

	if arg_present(wei) then begin
		zele = Elecomp(ele)
		aele = abctab[zele].a
		wei = num*aele
		wei = wei/total(wei)
	endif

	return
end