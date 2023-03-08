Pro CDC, key, fnam, code = cdc, decode = dcd, show = sho, _extra = _e

;+
; NAME:
;		CDC
; VERSION:
;		8.72
; PURPOSE:
;		Codes and decodes text.
; CATEGORY:
;		Utility.
; CALLING SEQUENCE:
;		CDC, KEY [,FNAM] [, keywords]
; INPUTS:
;	KEY
;		Scalar string, arbitrary, serves as the coding/decoding key.  Length
;		must be between 4 and 96 characters.
; OPTIONAL INPUT PARAMETERS:
;	FNAM
;		File name.  If not given will be querried for interactively.
; KEYWORD PARAMETERS:
; 	/CODE												| If neither of these
; 		Switch.  If set, the text in the file is coded 	| two is set, CDC will
; 		using the key.									| check the file
; 	/DECODE												| extension.  .TXC will
; 		Switch.  If set, the text in the file is decoded| force decoding, else
; 		using the key.									| default is coding.
; 	/SHOW
; 		Switch.  If set, while decoding, the decoded text is printed to the
; 		screen, else it is sent to a file.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Normally returns the list of routines called by RNAME, as string array.
;		If COMA_SEP is set, the string array is joined into a single string,
;		with comas separating the entries.  If no routines are found, returns
;		!NULL.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		On coding, combines the input key with the text to generate the coded
;		text, and writes the coded text into a file.  And decoding reads a file
;		reverses coding procedure and writes the decoded text into a file, or,
;		if /SHOW is set, writes the decoded text to the screen.
;		
;		 Note:  Decoding will only work if same key is used as the one that was
;		 		used for the coding.  Case matters.
;		 Calls DEFAULT, FILE_GET, FNAMPARSE, ONE_OF, RASCLINE, STREQ and
;		 STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-MAY-2012
;		Documented 25-DEC-2020 by Mati Meron.
;-

	on_error, 1
	wext = ['.txc','.txt']
	dext = strmid(wext,1)

	klen = strlen(Default(key,'',/dtyp))
	if klen ge 4 or klen le 96 then bkey = byte(key) $
	else message, 'Code string should have at least 4 characters!'

	ifnam = File_get(fnam,stat=istat,/read,_extra=_e)
	if istat then begin
		nam = Fnamparse(ifnam,pat=pat,ext=ext)
		dfl = One_of(cdc,dcd)
		if dfl eq -1 then dfl = Streq(ext,wext[0])
		ofnam = pat + nam + wext[dfl]

		if dfl then begin
			openr, iun, ifnam, /get_lun
			len = (fstat(iun)).size
			blin = bytarr(len)
			readu, iun, blin
			free_lun, iun
			check = blin[0:1]
			blin = blin[2:*]
			len = len-2
			bcheck = (byte(check) + 128b - bkey[-2:-1]) mod 128b
			shf = long(string(bcheck))
			skey = string(shift(bkey,shf))
			bkey = (byte(replicate(skey,len/klen+1)))[0:len-1]
			blin = (blin + 128b - bkey) mod 128b
			dum = Strparse_mm(string(blin),string(127b),lin)
			dum = where(lin eq ' ',ndum)
			if ndum gt[0] then lin[dum] = ''
			if keyword_set(sho) then begin
				fra = strjoin(replicate('#',80))
				print, fra
				print, lin, form='(a)'
				print, fra
			endif else begin
				ofnam = $
				File_get(ofnam,def=dext[dfl],/write,/over,stat=ostat,_extra=_e)
				if ostat then begin
					openw, oun, ofnam, /get_lun
					printf, oun, lin, form = '(a)'
					free_lun, oun
				endif else message, 'Error opening output file!'
			endelse
		endif else begin
			dum = Rascline(ifnam,lines=lin,count=con)
			if con gt 0 then begin
				dum = where(lin eq '',ndum)
				if ndum gt 0 then lin[dum] = ' '
				blin = byte(strjoin(lin + string(127b)))
				shf = round(0.5 + (klen-1)*randomu(s))
				pref = (byte(string(shf,form='(i02)')) + bkey[-2:-1]) mod 128b
				skey = string(shift(bkey,shf))
				len = n_elements(blin)
				bkey = (byte(replicate(skey,len/klen+1)))[0:len-1]
				blin = (blin + bkey) mod 128b
				blin = [pref,blin]
				ofnam = $
				File_get(ofnam,def=dext[dfl],/write,/over,stat=ostat,_extra=_e)
				if ostat then begin
					openw, oun, ofnam, /get_lun
					writeu, oun, blin
					free_lun, oun
				endif else message, 'Error opening output file!'
			endif else message, 'Empty file, nothing to code!'
		endelse
	endif else message, 'Error opening input file!'

	return
end