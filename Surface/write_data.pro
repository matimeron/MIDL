Pro Write_data, dat, filnam, header = hed, _extra = _e

;+
; NAME:
;		WRITE_DATA
; VERSION:
;		8.15
; PURPOSE:
;		Writes 1D and 2D data into a text file.
; CATEGORY:
;		Surface specific Input/Output.
; CALLING SEQUENCE:
;		WRITE_DATA, DAT, FILNAM [, keywords]
; INPUTS:
;	DAT
;		Surface data in one of the following forms:
;		
;			1D data	:	A [3,N] array in the [independent_variable, data, error]
;						format.  Optionally a [2,N] array is also acceptable 
;						and in this case the third (error) column is generated
;						internally.
;
;			2D data	:	A [4,N,M] array containg four "pages" in the 
;						[X,Y,data,error] format.  Optionally a [3,N,M] array is
;						also acceptable and in this case the fourth (error)
;						page is generated internally.
;			Partial
;			2D data	:	A [M,N] array containing the data alone.  In this case
;						the other 3 pages (X, Y, error) are generated 
;						internally.  Note that the first dimension, M, must be
;						> 3, to avoid confusion with 1D data.
;						
;			Any other input format will result in error.
; OPTIONAL INPUT PARAMETERS:
;	FILNAM
;		Char. value, the name of the data file.  Default extension is '.TXT'.
;		Even when a file name is provided, an interactive GUI will open to
;		allow the user to confirm the name and path.  This default action can be
;		overriden by using the keyword /AUTO which is passed by _EXTRA to
;		FILE_GET.  With /AUTO set, files are saved automatically, with no 
;		querries, except for the first call to WRITE_DATA, when the path hasn't
;		been established yet.  To avoid a query on the first call, a full name,
;		including path, should be given.
; KEYWORD PARAMETERS:
;	HEADER
;		Can be used in two ways, as follows:
;			1)	If given as character array, the entries in the array will be
;				used as column titles.
;			2)	If set as a switch, default headers will be used, namely:
;				for 1D data	:	'Q','Data','Errors'
;				for 2D data	:	'Q_xy','Q_z','Data','Errors'
;		If not present, the data will be written with no headers.
;	_EXTRA
;		A formal keyword, used to transfer keywords to embedded routines.  Of 
;		special interest are the following keywords:
;		
;			AUTO	-	See above, under FILNAM.
;			TITLE	-	Provides a title to the outup, above the header.
;			VERBOSE -	When set, the full name (including path) of the file
;						being written is printed to the screen.
; OUTPUTS:
;		None other than the file being written.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		See list of acceptable DAT formats, above.
; PROCEDURE:
;		Writes the data into a text file.  1D data is written in a 3 column
;		format while 2D data is converted into four columns, in X, Y, Data, 
;		Error order, with X advancing the fastest.
;		Calls ISNUM, TYPE and WASCII from MIDL.  Also calls itself, 
;		recursively.
; MODIFICATION HISTORY:
;		Created 1-MAR-2011 by Mati Meron.
;		Modified 15-FEB-2012 by Mati Meron.  Internal changes.
;-

	on_error, 1

	wrifl = 1
	siz = size(dat)
	case siz[0] of
		2	:	begin
					case siz[1] of
						1	:	message, 'Not a valid 1D data!'
						2	:	begin
									wdat = make_array(3,siz[2],type=Type(dat))
									wdat[0:1,*] = dat
									wdat[2,*] = sqrt(wdat[1,*] > 0)
								end
						3	:	wdat = dat
						else:	begin
									wdat=make_array([4,siz[1:2]],type=Type(dat))
									wdat[0,*,*] = $
										findgen(siz[1])#replicate(1.,siz[2])
									wdat[1,*,*] = $
										replicate(1.,siz[1])#findgen(siz[2])
									wdat[2,*,*] = dat
									wdat[3,*,*] = sqrt(dat > 0)
									Write_data, wdat, filnam, head=hed,_extra=_e
									wrifl = 0
								end
					endcase
					dhed = ['Q','Data','Errors']
				end
		3	:	begin
					if siz[1] ge 3 then begin
						len = siz[2]*siz[3]
						wdat = make_array(4,len,type=Type(dat))
						for i = 0, 2 do wdat[i,*] = $
							reform(transpose(dat[i,*,*]),len)
						if siz[1] gt 3 then wdat[3,*] = $
							reform(transpose(dat[3,*,*]),len) $
						else wdat[3,*] = sqrt(wdat[2,*] > 0)
					endif else message, 'Not a valid 2D data!'
					dhed = ['Q_xy','Q_z','Data','Errors']
				end
		else:	message, 'Not a valid data!'
	endcase

	if wrifl then begin
		if keyword_set(hed) then begin
			if Isnum(hed) then whed = dhed else whed = hed
			Wascii, wdat, filnam, head=whed, call=2,_extra=_e
		endif else Wascii, wdat, filnam, /nohead, call=2,_extra=_e
	endif

	return
end