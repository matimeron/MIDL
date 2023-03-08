Pro Beam_show, beam, x_vals = xvl, y_vals = yvl, full = ful, names = nam, $
	show_vals = shv, plot_vals = plv, standard= stn, relative= rel, $
	sigma = sig, fwhm= fwh, _extra = _e

;+
; NAME:
;		BEAM_SHOW
; VERSION:
;		6.4
; PURPOSE:
;		Displays (in tabular form) data from a beam structure of type OPBEAM.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = BEAM_SHOW( BEAM, keywords)
; INPUTS:
;	BEAM
;		Structure of type OPBEAM.  See OPBEAM__DEFINE for details.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/X_VALS
;		Switch.  When set, values for the X dimension of the beam are displayed.
;	/Y_VALS
;		Switch.  Same as X_VALS, for the Y dimension.
;
;		Note:	One and only one of X_VALS and Y_VALS must be specified.
;	/FULL
;		Switch.  If set, free propagation segments are considered optical
;		elements and the beam parameters at the *end* of each such segment are
;		displayed.  Else, free propagation segments are ignored.
;	/NAMES
;		Switch.  If set, the names of the optical elements are displayed.
;		Note:  this reduces the number of optional columns from 5 to 4.
;	SHOW_VALS
;		A list of data types to be displayed.
;
;		BEAM_SHOW *always* displays the following 3 columns:
;
;			a)	Location of the optical elements, measured from the physical
;				source.
;			b)	The optical elements at locations (a) (see list of possible
;				elements in BEAMSEC__DEFINE.
;			c)	Parameters of the elements in (b), one parameter per element.
;
;		If /NAMES (see above) is set, a column with the names of the elements
;		is also displayed, *preceding* the 3 columns above.
;
;		In addition, up to 5 (4 if /NAMES is set) additional columns may be
;		displayed, chosen out of the following list (in order)
;
;			0)  ZSORC:	Current location of the virtual source (m)
;			1)  SSIZE:  Current value of source size (micron).
;			2)  ASIZE:	Current value of source angular size (microradian).
;			3)  HSIZE:	Current value of beam size (micron)
;			4)  COLEN:	Current value of coherence length (micron).
;			5)	CRLEN:	Current value of correlation length (micron).
;			6)	EMITT:	Current value of emittance (micron*microradian).
;			7)	RFLUX:	Current value of beam flux (relative to initial flux).
;			8)	RBRIL:	Current value of brilliance (relative to initial).
;
;		SHOW_VALS provides either an integer vector (up to 5 values in the 0-7
;		range, or a string vector (up to five names from the list above, only
;		first 3 letters are needed).
;
;		Note 1:	All "current values" are evaluated at the corresponding location
;		Note 2:	The relative flux (in 7) is for the dimension displayed,
;				independent of any flux changes along the other dimension.
;	PLOT_VALS
;		A name of a (single) data type to be plotted as a function of distance
;		from the source.
;	/STANDARD
;		Switch.  If set and if SHOW_VALS is not given, a "standard" list of
;		show values including [ZSORC,SSIZE,ASIZE,HSIZE,RFLUX] is used.  The
;		last item (RFLUX) will be dropped if /NAMES is set.
;		If STANDARD is set and equals 2 then RFLUX is replaced with RBRIL.
;	/RELATIVE
;		Switch.  If set, the values of COLEN, CRLEN and EMITT are given as
;		relative values (dimensionless).  In such case COLEN and CRLEN are
;		normalized to beam size (HSIZE) and EMITT is normalized to wavelength
;	/SIGMA
;		Switch.  If set, the values of SSIZE, ASIZE, HSIZE,	COLEN,	|  One and
;		 CRLEN and EMITT are given as Sigma values (or the square	|  Only one
;		of this for emittance).  Default is "full values" i.e. 		|  of these
;		sqrt(4*!pi)*sigma (or the square of this for emittance).	|  two
;	/FWHM															|  keywords
;		Switch.  If set, the values of SSIZE, ASIZE, HSIZE, COLEN,	|  can be
;		CRLEN and EMITT are given as FWHM values i.e. 				|  set.
;		sqrt(8*alog(2))*sigma (or the square of this for emittance).|
;	_EXTRA
;		A formal key, used to transfer  keywords to PLOT.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls ISNUM, ONE_OFF, STREQ, STRMATCH_MM and TABULATE
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 30-MAY-2002 by Mati Meron as SHOW_BEAM.
;		Modified 10-NOV-2002 by Mati Meron.
;		Modified 5-DEC-2006 by Mati Meron.  Renamed BEAM SHOW, added keywords
;		/STANDARD and /FWHM plus internal changes.
;		Modified 15-DEC-2006 by Mati Meron.  Added new callable function, RBRIL.
;		Modified 10-SEP-2007 by Mati Meron.  Added keyword /SIGMA.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	showdef = 3
	showmax = 8

	col =  strcompress('c' + sindgen(showmax),/remove)
	posib = ['zsorc', 'ssize', 'asize', 'hsize',$
			'colen', 'crlen', 'emitt', 'rflux', 'rbril']
	ctoext = ['dif','ssl','asl']
	ahead = ['elem','loc (m)','param']
	head = ['sor (m)', 'ss (mcm)', 'as (mcr)', 'bs (mcm)',$
			'col (mcm)', 'crl (mcm)', 'em (mc^2)', 'rflux', 'rbril']
	rhead = ['sor (m)', 'ss (mcm)', 'as (mcr)', 'bs (mcm)',$
			'rel_col', 'rel_crl', 'rel_emt', 'rflux', 'rbril']
	aform = ['a8','f8.3','f10.3']
	form = ['f8.3','f8.3','f8.3','f8.3','f8.3','f8.3','f8.3','f8.6','f8.6']
	mult = replicate(1.,9)
	dum = One_of(sig,fwh)
	if dum ge 0 then begin
		if dum eq 0 then rat = 1/sqrt(4*!pi) else rat = sqrt(2*alog(2)/!pi)
		mult[1:5] = rat
		mult[6] = rat^2
	endif
	if keyword_set(rel) then begin
		mult[4:6] = 1.
		head = rhead
	endif

	case One_of(xvl,yvl) of
		0	:	begin
					if beam.set and 1 ne 0 then wsec = beam.xsec $
					else message, 'X-beam not defined'
					pref = 'X-'
				end
		1	:	begin
					if beam.set and 2 ne 0 then wsec = beam.ysec $
					else message, 'Y-beam not defined'
					pref = 'Y-'
				end
		else:	message, 'Have to specify, either X or Y!'
	endcase
	tit =  pref + 'beam propagation at wavelength of '+ $
	string(beam.wavl/mwlen, form='(f5.2)') + ' angstrem.'

	nel = 2*beam.npoints - 1
	inds = lindgen(nel)
	wsec = wsec[inds]
	bsec = wsec.bpars
	npl = n_elements(plv)
	if npl eq 1 then begin
		if Isnum(plv) then wplv = long(plv) else wplv = Strmatch_mm(plv,posib,3)
		if wplv lt 0 or wplv gt n_elements(posib) - 1 $
		then message, 'Unacceptable plot values!'
		if wplv eq 0 then tem = wsec.zval else tem = beam.wavl
		disp = call_function(posib[wplv],bsec,tem,rel=rel)
		plot, wsec.zval, disp, tit = tit + '  ' + head(wplv) + '.', _extra =_e
	endif else if npl ne 0 then message, 'Only single plots allowed', /continue

	if not keyword_set(ful) then begin
		nel = beam.npoints
		inds = 2*lindgen(nel)
		wsec = wsec[inds]
		bsec = wsec.bpars
	endif

	c0 = wsec.optel
	c1 = wsec.zval
	c2 = wsec.optpar
	dum = where(wsec.elset eq 0,ndum)
	if ndum gt 0 then begin
		c0[dum] = 'off'
		c2[dum] = 0
	endif
	conv = bytarr(nel)
	for i = 0l, n_elements(ctoext) - 1 do conv = conv or Streq(ctoext[i],c0,3)
	dum = where(conv ne 0,ndum)
	if ndum gt 0 then c2[dum] = c2[dum]/mmicr

	if keyword_set(nam) then begin
		c3 = c2
		c2 = c1
		c1 = c0
		c0 = beam.elname[inds]
		ahead = ['name',ahead]
		aform = ['a8',aform]
		showdef = showdef + 1
	endif else c0 = string(indgen(nel),form='(i3)') + ' ' + c0

	net = showmax - showdef

	nsh = n_elements(shv)
	if nsh eq 0 and keyword_set(stn) then begin
		if stn eq 1 then shv = posib[[0,1,2,3,7]] $
		else if stn eq 2 then shv = posib[[0,1,2,3,8]]
		shv = shv[0:net-1]
		nsh = net
	endif
	if nsh gt net then message, 'Too many show values!'
	if nsh gt 0 then begin
		if not Isnum(shv) then begin
			wshv = lonarr(nsh)
			for i = 0, nsh-1 do wshv[i] = Strmatch_mm(shv[i],posib,3)
		endif else wshv = long(shv)
		mish = min(wshv,max=mash)
		if mish lt 0 or mash gt n_elements(posib) - 1 $
		then message, 'Unacceptable show values!'
		wcol = col[showdef:showdef+nsh-1]
		wmlt = mult[wshv]
		tem = replicate('beam.wavl',nsh)
		dum = where(wshv eq 0,ndum)
		if ndum gt 0 then tem[dum] = 'wsec.zval'
		si = sindgen(nsh)
		com = '("' + posib[wshv] + '"' + ',bsec,' + tem + ',rel=rel)'
		for i = 0l, nsh - 1 do $
		dum = execute(wcol[i] + ' = wmlt['+ si[i] + ']*call_function' + com[i])
		ahead = [ahead,head[wshv]]
		aform = [aform,form[wshv]]
	endif

	print
	Tabulate, c0,c1,c2,c3,c4,c5,c6,c7,head=ahead,form=aform,tit=tit,/idf

	return
end