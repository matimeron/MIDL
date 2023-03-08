Function Beam_read, filnam, wavelength = wav, energy = ene, show_head = sho, $
	_extra = _e

;+
; NAME:
;		BEAM_READ
; VERSION:
;		5.6
; PURPOSE:
;		Fills up a beam structure of type OPBEAM from file data.
; CATEGORY:
;		Optical calculations Input/Output.
; CALLING SEQUENCE:
;		Result = BEAM_READ( FILNAM, keywords)
; INPUTS:
;	FILNAM
;		Input file name.  If not given, quereed for interactively.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	WAVELENGTH														|
;		Numerical scalar, the wavelength value (in Angstrem) to be	|	At most
;		used.  If given, overrides whatever value is provided by	|	one of
;		the file.													|	these
;	ENERGY															|	two may
;		Numerical scalar, the energy value (in keV) to be used.	If	|	be used
;		given, overrides whatever value is provided by the file		|
;	/SHOW_HEAD
;		Switch.  If set, the data header (see explanation in PROCEDURE) is
;		printed to the screen.
;	_EXTRA
;		Formal keyword for transfering keywords to BEAM_MAKE.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a structure of type OPBEAM, with the fields filled up according
;		to the input values present in the file.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the data file satisfying the requirements (see below).
; PROCEDURE:
;		The optical data file is a text file  containing two blocks.
;
;		A)	Source block, starting with "begin_source" and ending with
;			"end_source" (anything following either of these two on the same
;			line will be ignored).  In between these two lines there *must* be:
;				1)	Line starting with "x_source" followed by two numerical
;					values representing the spatial and angu source sizes.
;				2)	A similar line starting with "y_source", for the Y sizes.
;			In addition, there may also be any of the following:
;				3)	A line starting with "energy" *or* a line starting with
;					"wavelength" (never both) followed by numerical value
;					representing energy (in keV) or wavelength (in angstrem).
;				4)	A line starting with "sigma" followed by 0 or 1.  0 value
;					signifies that the source data is given as "full size",
;					1-value signifies that the vlaues given are sigma values.
;					default is "full size".
;				5)	A line starting with "ind_len" followed by undulator length
;					in meters (edefault is 0).
;				6)	A line starting with "name" followed by beam name.
;				7)	a line starting with "sorname" followed by source name.
;			Any other line present within the block will be ignored.
;		B)	Beam block, starting with "begin_beam" and ending with "end_beam".
;			(anything following either of these two on the same line will be
;			ignored).  Between these two lines there may be an arbitrary (but
;			at least one) number of entries of the form:
;
;			Name  Type  Location  Parameter  Code  Set
;
;			separated by any combination of space, tabs and commas, where:
;				1)	Name:	Element name.  See field ELNAME in OPBEAM__DEFINE.
;				2)	Type:	See field ZVAL in BEAMSEC__DEFINE.
;				3)	Location:	See field OPTEL in BEAMSEC__DEFINE.
;				4)	Parameter:	See field OPTPAR in BEAMSEC__DEFINE.
;				5)	Code:	See field OPTCODE in BEAMSEC__DEFINE.
;				6)	Set:	See field ELSET in BEAMSEC__DEFINE.  This field is
;							optional, defaults to 1 if not given.
;
;		Any lines preceding the Source block are considered header and will be
;		printed tothe screen if /SHOW_HEAD (see above) is set.  Any lines
;		following the Beam block are ignored.
;
;		Note:	If the element type is DIF and the Code is 1, the Parameter
;				value is taken as slope error to be translated into correlation
;				length as Cor_len = wavelength/slope_error.
;
;		Reads the relevant data and generates an OPBEAM type structure through
;		calls to BEAM_INIT and BEAM_MAKE.  Also calls ARREQ, DEFAULT, ISNUM,
;		ONE_OF, RASCLINE, STREQ and STRPARSE_MM from MIDL
; MODIFICATION HISTORY:
;		Created 10-DEC-2006 by Mati Meron.
;		Modified 15-JAN-2007 by Mati Meron.  Added keyword ENERGY.
;-

	on_error, 1

	scod = ['begin_source','end_source','begin_beam','end_beam']
	lcod = strlen(scod)

	pnt = Rascline(filnam,stat=sta,lines=dat,count=nl,call=2)
	if sta then begin
		dat = strtrim(dat,1)
		slin = lonarr(4)
		for i = 0, 3 do begin
			dum = where(Streq(dat,scod[i],lcod[i]),ndum)
			if ndum eq 1 then slin[i] = dum else message, 'Bad data file!'
		endfor
		if not Arreq(sort(slin),lindgen(4)) then message,'Disordered data file!'

		if keyword_set(sho) and (slin[0] gt 0) then begin
			print
			print, dat[0:slin[0]-1], form='(a)'
		endif

		if One_of(wav,ene) eq 1 then wav = !srcon.conv/ene

		if (slin[1] - slin[0]) gt 2 then begin
			sdat = dat[slin[0]+1:slin[1]-1]
			dumf = (where(Streq(sdat,'wave',4),ndumf))[0]
			dums = (where(Streq(sdat,'energy',6),ndums))[0]
			if ndumf gt 1 or ndums gt 1 then message, 'Single values only!'
			case ndumf + 2*ndums of
				0	:	wavl = 1.
				1	:	begin
							tem = Strparse_mm(sdat[dumf],' 	,=',lis)
							wavl = float(lis[1])
						end
				2	:	begin
							tem = Strparse_mm(sdat[dums],' 	,=',lis)
							wavl = !srcon.conv/float(lis[1])
						end
				3	:	message, 'Either wavelength or energy, not both!'
			endcase
			wavl = Default(wav,wavl,/dtyp)
			dum = (where(Streq(sdat,'x_source',8),ndum))[0]
			if ndum eq 1 then begin
				tem = Strparse_mm(sdat[dum],' 	,=',lis)
				wxsr = float(lis[1:2])
			endif
			dum = (where(Streq(sdat,'y_source',8),ndum))[0]
			if ndum eq 1 then begin
				tem = Strparse_mm(sdat[dum],' 	,=',lis)
				wysr = float(lis[1:2])
			endif
			if (not Isnum(wxsr)) and (not Isnum(wysr)) then $
			message, 'Missing or corrupted sources!'

			dum = (where(Streq(sdat,'sigma',5),ndum))[0]
			if ndum eq 1 then begin
				tem = Strparse_mm(sdat[dum],' 	,=',lis)
				wsig = fix(lis[1])
			endif else wsig = 0
			dum = (where(Streq(sdat,'und_length',7),ndum))[0]
			if ndum eq 1 then begin
				tem = Strparse_mm(sdat[dum],' 	,=',lis)
				wudl = float(lis[1])
			endif
			dum = (where(Streq(sdat,'name',4),ndum))[0]
			if ndum eq 1 then begin
				tem = Strparse_mm(sdat[dum],' 	,=',lis)
				wnam = lis[1]
			endif
			dum = (where(Streq(sdat,'sorname',7),ndum))[0]
			if ndum eq 1 then begin
				tem = Strparse_mm(sdat[dum],' 	,=',lis)
				wsnm = lis[1]
			endif
		endif else message, 'No source data present!'

		if (slin[3] - slin[2]) gt 2 then begin
			bdat = dat[slin[2]+1:slin[3]-1]
			n = n_elements(bdat)
			bnam = (bele = strarr(n))
			bloc = (bpar = fltarr(n))
			bcod = (bset = intarr(n))
			bset = bset + 1
			j = 0
			for i = 0l, n-1 do begin
				dum = Strparse_mm(bdat[i],' 	,',lis)
				if dum ge 4 then begin
					bnam[j] = lis[0]
					bele[j] = lis[1]
					bloc[j] = float(lis[2])
					bpar[j] = float(lis[3])
					bcod[j] = fix(lis[4])
					if dum gt 4 then bset[j] = fix(lis[5])
					j = j + 1
				endif
			endfor
			nn = j - 1
			if nn gt 0 then begin
				bnam = bnam[0:nn]
				bele = bele[0:nn]
				bloc = bloc[0:nn]
				bpar = bpar[0:nn]
				bcod = bcod[0:nn]
				bset = bset[0:nn]
			endif else message, 'Beam data missing!'
		endif else message, 'No beam data present!'

		beam = Beam_init(wavl,x_source=wxsr,y_source=wysr,$
			sig=wsig,und=wudl,nam=wnam,sorname=wsnm)
		beam = Beam_make(beam,ele=bele,loc=bloc,para=bpar,code=bcod,elset=bset,$
				elnam=bnam,sigma=wsig,_extra=_e)
	endif else message, 'No file!'

	return, beam
end