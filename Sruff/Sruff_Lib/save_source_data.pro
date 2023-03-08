Pro Save_source_data, infile, outfile

;+
; NAME:
;		SAVE_SOURCE_DATA
; VERSION:
;		8.213
; PURPOSE:
;		Reads an input ASCII file of of synchrotron source data, converts it
;		into a structure and writes it out as a binary XDR file.
; CATEGORY:
;		X-ray utility.
; CALLING SEQUENCE:
;		SAVE_SOURCE_DATA [, INFILE [, OUTFILE]]
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;	INFILE:
;		The name of the ASCII file containing the synchrotron source data.  
;		Default is sruff_data\sync_sources.dat where sruff_data is an
;		environment variable pointing to the \sruff\data subdirectory in MIDL.
;	OUTFILE
;		Name of output XDR binary file.  Default is same as INFILE, with an
;		extension of .SAV instead of .DAT.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Reads the data and creates an array of structures of type SOURCE, one
;		structure for each crystal.  Each SOURCE structure contains, in order:
;			SYNC  -	Synchrotron's name.
;			DESC  -	One sentence description of the source.
;			RENE  -	Ring energy in GeV.
;			RGAM  - The relativistic GAMMA of the machine.
;			CURR  -	The current in the machine in Amperes.
;			BMMF  -	Bending magnet magnetic field, in Tesla.
;			DLEN  -	The standard insertion device length in meters.
;			SLEN  - Tthe standard superconducting device length in meters.
;			MGAP  -	Minimal insertion device gap, in mm.
;			SGAP  -	Superconducting device gap, in mm.
;			RSIG  -	Source (ID) spatial dimensions in mm.  2-element vector
;					in a [sigma_x,sigma_y] format.
;			ASIG  -	Source (ID) angular dimensions in mrad.  2-element vector
;					in a [sigma_x',sigma_y'] format.
;			APER  -	The standard front end aperture size in mm.  2-element
;					vector in a [xsize,ysize] format.
;			DIST  -	The standard distance from source to the aperture in meters.
;		All the numerical values are given in a floating format.
;
;		Uses calls to DEFAULT, RASCLINE, STREQ, STRMATCH_MM and STRPARSE_MM, 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-OCT-2002 by Mati Meron, based on the SAVE_ABS_COEFFS routine.
;-

	on_error, 1

	fname = getenv('sruff_data') + 'sync_sources'
	infile  = Default(infile,fname + '.dat')
	outfile = Default(outfile,fname + '.sav')

	tlis = tag_names({source})
	ntag = n_elements(tlis)
	clen = 4

	pnt = Rascline(infile,stat=sta,lines=dat,call=2)
	if sta then begin
		dat = strtrim(dat,2)
		be = where(Streq(dat,'begin'),nbe)
		en = where(Streq(dat,'end'),nen)
		if nbe eq nen then begin
			sortab = replicate({source},nbe)
			for i = 0, nbe-1 do begin
				cur = {source}
				for j = be[i]+1, en[i]-1 do begin
					whi = Strmatch_mm(dat[j],tlis,clen)
					if whi ge 0 then begin
						dum = Strparse_mm(dat[j],'	',lis)
						cur.(whi) = lis[1:*]
					endif
				endfor
				sortab[i] = cur
			endfor
			save, file=outfile, /XDR, sortab
		endif else message, 'Bad or corrupted input file!'
	endif else message, 'File not found!'

	return
end