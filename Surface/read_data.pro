Function Read_data, filnam, twod = twd, alternate = alt, _extra = _e

;+
; NAME:
;		READ_DATA
; VERSION:
;		8.21
; PURPOSE:
;		Reads 1D and 2D data from text files wtitten by WRITE_DATA.
; CATEGORY:
;		Surface specific Input/Output.
; CALLING SEQUENCE:
;		Result = READ_DATA( FILNAM [, keywords])
; INPUTS:
;	FILNAM
;		Char. value, the name of the data file.  Default extension is '.TXT'.
;		If not provided, or if file with the provided name cannot be found, an 
;		interactive GUI will open to allow the user to select the file. 
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/TWOD
; 		Switch.  Specifies that the data is 2D.  By default 3-column data is
; 		assumed 1D while 4-column data is taken as 2D.  TWOD overrides this
; 		convention for 3-column data (makes no difference for 4-column data.
; 	/ALTERNATE
; 		Switch.  Specifies "alternative order" for 2D data.  The default (for
; 		historic reasons) is "change Y then X".  /ALTERNATE reverses this order.
;	_EXTRA
;		A formal keyword, used to transfer keywords to embedded routines.
; OUTPUTS:
;		Returns, in standard form, the data contained in the file.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The numeric data in the file must be in either 3 column (1D data) or
;		4 column (2D data) format, any other format will result in error.
;		
;		Note: if /TWOD is set, 3-column data will be read as 2D data.
; PROCEDURE:
;		Reads the data from the text file into a variable.  1D data is read as
;		is, 2D data is converted on read from 4 columns to 4 "pages".
;		Calls RASCII from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2011 by Mati Meron.
;		Modified 30-JUN-2013 by Mati Meron.  Added keywords TWOD and ALTERNATE.
;-

	on_error, 1

	rdat = Rascii(filnam,npo=npo,filt='txt',call=2,_extra=_e)
	if npo[0] eq 3 or npo[0] eq 4 then tdfl= (npo[0] eq 4) or keyword_set(twd) $
	else message, 'Not a valid data!'

	if tdfl then begin
		if keyword_set(alt) then begin
			fir = (where(rdat[1,*] ne rdat[1,0]))[0]
			sec = npo[1]/fir
			res = fltarr(npo[0],fir,sec)
			for i = 0, npo[0]-1 do res[i,*,*] = reform(rdat[i,*],fir,sec)
		endif else begin
			sec = (where(rdat[0,*] ne rdat[0,0]))[0]
			fir = npo[1]/sec
			res = fltarr(npo[0],fir,sec)
			for i = 0, npo[0]-1 do $
			res[i,*,*] = transpose(reform(rdat[i,*],sec,fir))
		endelse
	endif else res = rdat

	return, res
end