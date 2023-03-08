Function Read_data_old, filnam, _extra = _e

;+
; NAME:
;		READ_DATA
; VERSION:
;		8.02
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
; PROCEDURE:
;		Reads the data from the text file into a variable.  1D data is read as
;		is, 2D data is converted on read from 4 columns to 4 "pages".
;		Calls RASCII from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2011 by Mati Meron.
;-

	on_error, 1

	rdat = Rascii(filnam,npo=npo,filt='txt',call=2,_extra=_e)
	case npo[0] of
		3	:	res = rdat
		4	:	begin
					sec = (where(rdat[0,*] ne rdat[0,0]))[0]
					fir = npo[1]/sec
					res = fltarr(npo[0],fir,sec)
					for i = 0, npo[0]-1 do $
					res[i,*,*] = transpose(reform(rdat[i,*],sec,fir))
				end
		else:	message, 'Not a valid data!'
	endcase

	return, res
end