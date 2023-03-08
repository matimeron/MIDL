Pro Save_cryst_data, infile, outfile

;+
; NAME:
;		SAVE_CRYST_DATA
; VERSION:
;		4.3
; PURPOSE:
;		Reads an input ASCII file of monochromator crystal data and writes it
;		out as a binary XDR file.
; CATEGORY:
;		X-ray calculations
; CALLING SEQUENCE:
;		SAVE_CRYST_DATA [, INFILE [, OUTFILE]]
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;	INFILE:
;		The name of the ASCII file containing the crystal data.  Default is
;		mono_data\crystal_data.dat where mono_data is an environment variable
;		pointing to the mono|data subdirectory in MIDL.
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
;		MONO_STUFF.  Contains the following:
;			NFL	:		Flag.  When set to zero, signals to LOAD_CRYST_DATA
;						to reload the tables.
;			CONV:		Conversion constant from wavelength to energy.
;						Equals 12.398424 (Angstrem*keV)
;			CER:		The classic electron radius.
;						Equals 2.8179409e-5 (Angstrem)
;			ASFTAB:		Structure, containing crystal data.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Reads the data and creates an array of structures of type CDAT, one
;		structure for each crystal.  Each CDAT structure containes, in order:
;			NAME	-	String, crystal's name.
;			CSYM	-	String, chemical symbol		|	Meaningful only for
;			Z		-	Integer, element's Z.		|	elemental crystals.
;			A		-	Float, element's A.			|
;			RO		-	Float, crystal's density in gr/cm^3.
;			CTYP	-	Float, structure type.
;			CSIZ	-	Float, 3-element vector, structure dimensions in
;						Angstrem
;			FLEN	-	Long, length of the following table.  Max. value is 32.
;			FTAB	-	An (FLEN,3) float array containing the spline
;						coefficients that are needed to calculate the atomic
;						scattering factor for any k value.  For information
;						about the spline array, see routines SPLIN_COEFFS and
;						SPLIN_EVAL in MIDL.
;
;		Uses calls to DEFAULT, STRMATCH_MM, STRPARSE_MM and SPLIN_COEFFS in
;		MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2002 by Mati Meron, based on the SAVE_ABS_COEFFS routine
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1
	nfl = 0
	conv = 12.398424
	cer = 2.8179409e-5

	fname = getenv('mono_data') + 'crystal_data'
	infile  = Default(infile,fname + '.dat')
	outfile = Default(outfile,fname + '.sav')

	cmx = 32
	blank = {cdat, name: '', csym: '', z: 0, a: 0., ro: 0., $
	ctyp: '', csiz: fltarr(3), flen: 0l, ftab: fltarr(cmx,3)}
	kwords = ['begin', 'end']

	on_ioerror, file_no_good
	openr, datun, infile, /get_lun
	nli = -1
	datfl = 0
	line = ''
	while not eof(datun) do begin
		readf, datun, line
		if line ne '' then begin
			mlast = Strparse_mm(line,'	 ',list)
			mnum = Strmatch_mm(list[0],kwords,3)
			case (datfl + mnum + 1) of
				1	: begin
						nli = nli + 1
						if nli eq 0 then asftab=blank $
						else asftab=[asftab,blank]
						asftab[nli].name = list[1]
						asftab[nli].csym = list[2]
						asftab[nli].z = list[3]
						asftab[nli].a = list[4]
						asftab[nli].ro = list[5]
						asftab[nli].ctyp = list[6]
						asftab[nli].csiz = list[7:9]
						hold = fltarr(cmx,2)
						datfl = 3
						npo = -1
					  end
				2	: message, 'END without BEGIN, aborting!'
				3	: begin
						npo = npo + 1
						if npo eq cmx then message, 'Data overflow!'
						hold[npo,0:1] = float(list[0:1])
					  end
				4	: message, 'BEGIN without END, aborting!'
				5	: begin
						asftab[nli].flen = npo
						use = hold[0:npo,*]
						asftab[nli].ftab[0:npo,*] = $
						Splin_coeffs(use[*,0],use[*,1])
						datfl = 0
					  end
				else:
			endcase
		endif
	endwhile
	free_lun, datun
	asftab.name = strcompress(asftab.name,/remove_all)
	asftab.csym = strcompress(asftab.csym,/remove_all)
	asftab.ctyp = strcompress(asftab.ctyp,/remove_all)
	save, file=outfile, /XDR, conv, cer, asftab
	return

	file_no_good:
	free_lun, datun
	return
end