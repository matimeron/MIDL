Function Read_xyz, file, particles= prt, timesteps= tms, save= sav, _extra = _e

;+
; NAME:
;		READ_XYZ
; VERSION:
;		8.422
; PURPOSE:
;		Reads data from an .XYZ file into an array.
; CATEGORY:
;		Input/Output, specific.
; CALLING SEQUENCE:
;		Result = READ_XYZ( FILE [, optional keywords])
; INPUTS:
;	FILE
;		Char. value, the name of the input file.  Strictly speaking, FILE
;		is optional.  The rules regarding FILE are:
;			1)	If full filename, including path, is given, it'll be used as is.
;			2)	If "bare" name, without path, is given, it is assumed the file
;				resides in the Current Directory.
;			3)	If FILE is not given, or if it is given but no file with such
;				name is found, the user is asked to point to the required file,
;				interactively.
;			4)	If in cases (1-2) the file name given lacks an extension, the
;				extension '.xyz' will be used.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	PARTICLES
; 		Optional output, see below.
; 	TIMESTEPS
; 		Optional output, see below.
; 	/SAVE
; 		Switch.  If set, the data is saved in a GDF file.  The name and location
; 		of the saved file is same as the input file, except for the extension
; 		which is '.gdf'
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the data in a [6,N] floating format, where N is the number of
;		data lines in the .xyz file.  Columns 0:2 of the output are columns 1:3
;		of the input, columns 3:4 are blank.  Column 5 contains timestep #.
; OPTIONAL OUTPUT PARAMETERS:
;	PARTICLES
;		Returns the number of particles in the input file, as a long integer.
;	TIMESTEPS
;		Returns the number of timesteps in the input file, as a long integer.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than that the input file must be a valid XYZ file.
; PROCEDURE:
;		Straightforward.  Calls FILE_GET, FNAMPARSE, RASCII, STREQ and TYPE,
;		from MIDL.  If needed, calls WRITE GDF.
; MODIFICATION HISTORY:
;		Created 10-AUG-2015 by Mati Meron.
;-

	on_error, 1

	if Type(file) eq 7 then begin
		fnam = Fnamparse(file,pat=pat,ext=ext)
		cur = Streq(pat,'')
		if Streq(ext,'') then wfile = file + '.xyz' else wfile = file
	endif
	wfile = File_get(wfile,cur=cur,stat=sta,/read,_extra=_e)

	if sta then begin
		openr, readun, wfile, /get_lun
		line = ''
		readf, readun, line
		prt = long(line)
		free_lun, readun

		pres = Rascii(wfile,skip=2,buf=prt,npo=npo)
		len = npo[1]
		tms = len/prt
		res = fltarr(6,len)
		res[0:2,*] = pres[1:3,*]
		for i = 0l, tms-1 do res[5,i*prt:(i+1)*prt-1] = i

		if keyword_set(sav) then begin
			fnam = Fnamparse(wfile,pat=pat,ext=ext)
			sfile = pat + fnam + '.gdf'
			write_gdf, res, sfile, _extra= _e
		endif
	endif else message, 'Input file problems!' 

	return, res
end