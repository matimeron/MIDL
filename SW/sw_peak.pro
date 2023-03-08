Function SW_peak, snum, eran= ern, roi= roi, pick= prn, force= frc, angle= ang,$
	check= chk,ret_range= ret, show= sho, output= oup, png= png, change= chn, $
	_extra= _e

;+
; NAME:
;		SW_PEAK
; VERSION:
;		7.15
; PURPOSE:
;		Reads and patches together data from MCA scans.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = SW_PEAK( SNUM [,keywords])
; INPUTS:
;	SNUM
;		Scan list can be of any list form (i.e. scan numbers, not actual scans)
;		acceptable by SCAN_LIST_VER (see there).
;
;		Note:	The ordering of scan numbers in the list is arbitrary, for the
;				purpose of patching.  However, if the region of interest is
;				chosen interactively (see /ROI), the first scan on the list
;				will be used to make the selection
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ERAN
;		Energy range, given as a 2-parameter vector, specifies	|	One and
;		region of interest.										|	only one of
;	/ROI														|	these two
;		Switch, allows for an interactive selection of region 	|	must be
;		of interest.											|	used
;	/PICK
;		Switch.  If set, the user is asked to enter low and hi limits for the
;		energy range within which the region of interest is to be selected.
;		Only active when /ROI is set.
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	/ANGLE
;		Switch.  Modifies the behavior of the SCAN_MCA_READ function which reads
;		in the data.  By default, the vector MCA data is read as a function of
;		QZ (L, in SPEC).  If /ANGLE is set, the data is read as function of
;		"angle" (by convention the first column of the scan).
;	CHECK
;		Character input, allows for data verification by calculating and
;		displaying one characteristic of the selected peak (other than then
;		number of counts which is processed automatically.  Currently, the
;		recognized characteristics are:
;
;			CENT	:	Peak center
;			FWHM	:	Peak full width at half magnitude.
;			KURT	:	Peak kurtosis.
;			SHINF	:	Peak Shannon information content.
;			SIG		:	Peak sigma.
;			SKEW	:	Peak skewness.
;
;		Only first 3 character matter at input.  Case doesn't matter.
;	RET_RANGE
;		Optional output, see below.
;	/SHO
;		Switch.  If set, the patched result is plotted to the screen (and if
;		CHECK is used, its results are plotted as well, in a separate window.
;	/OUTPUT
;		Switch.  If set, printer output is performed.
;	/PNG
;		Switch.  If set, a PNG file of the plot is generated.
;	/CHANGE
;		Switch.  If set, a new directory for saving the PNG files is
;		established interactively.
;
;		Note:  If /SHO is not set, /OUTPUT, /PNG and /CHANGE are ignored.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to imbedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Returns the results in the standard 3-column scan format where the
;		columns contain:
;			0	:	Q_z values corresponding to consecutive points.
;			1	:	The number of counts for each Q_z.
;			2	:	The statistical errors of (1).
;		of Q_z.  Additional outputs are provided through the output parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_RANGE
;		Returns the boundaries of the range used by PEAK_STRIP (in XRANGE units)
;		as a 2-element vector, [low_limit, high_limit].
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the scans on the list must in fact exist and contain
;		MCA data.
; PROCEDURE:
;		Reads the scans, sums the counts within the region of interest
;		subtracting background and combines all the data points.
;		Calls SCAN_JOIN, SCAN_LIST_VER, SCAN_MCAO_READ, SCAN_ORDER and
;		SPEC_FILE_INFO, as well as PEAK_CENT, PEAK_COUNT, PEAK_SHOW and
;		PEAK_STRIP, from SPEC.  If required may call PEAK_FWHM, PEAK_KURT,
;		PEAK_SHINF, PEAK_SIG and PEAK_SKEW, from SPEC.  Also calls ARREQ,
;		ONE_OF, STRMATCH_MM, TRUCOL and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2004 by Mati Meron.
;		Modified 10-OCT-2005 by Mati Meron.  Added keyword ANGLE.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	posib = ['cent','fwhm','kurt','shinf','sig','skew']
	ctit = ['Center','FWHM','Kurtosis','Shannon Information','Sigma','Skew']
	tnams =  strcompress('t_' + sindgen(8),/remove)
	cnams =  strcompress('c_' + sindgen(8),/remove)

	rofl = One_of(ern,roi)
	if rofl lt 0 then message, 'No range given!'
	shofl = keyword_set(sho)

	if keyword_set(chk) then begin
		cwhi = Strmatch_mm(chk,posib,3)
		if cwhi ge 0 then begin
			cfun = 'Peak_' + posib[cwhi]
			cfl = 1
		endif else begin
			message, 'Check option ' + chk + ' does not exist!', /con
			cfl = 0
		endelse
	endif else cfl = 0

	Spec_file_info, _extra = _e
	dum = (Wherinstruct('new',_e))[0]
	if dum ge 0 then _e.(dum) = 0

	nsc = Scan_list_ver(snum,lis=slis,flag=sfl)
	if not (nsc gt 0 and sfl) then message, 'Missing or illegitimate input!'
	ver = bytarr(nsc)
	com = tnams + ' = ttem'
	if cfl then com = com + ' & ' + cnams + ' = ctem'

	for i = 0l, nsc-1 do begin
		dat = Scan_MCAO_read(slis[i],angle=ang,energy= eni,qz= qz,_extra= _e)
		if i eq 0 then begin
			en = eni
			tdat = total(dat,1)
			kdum = Peak_strip(en,tdat,/sta,xran=ern,roi=roi,pick=prn,/bran,$
				ret=ret,/sho,scol=Trucol(0,4,4),_extra=_e)
			if rofl then wern = ret else wern = ern
			if shofl then wait, 1
		endif
		ver[i] = Arreq(en,eni)
		nq = n_elements(qz)
		ttem = fltarr(3,nq)
		ttem[0,*] = qz
		if cfl then ctem = ttem
		for j = 0l, nq-1 do begin
			ttem[1,j]= Peak_count(en,dat[j,*],/sta,xran=wern,/bran,err=err)
			ttem[2,j] = err
			if cfl then begin
				ctem[1,j] = $
				call_function(cfun,en,dat[j,*],/sta,xra=wern,/bra,/pos,err=cerr)
				ctem[2,j] = cerr
			endif
		endfor
		dum = execute(com[i])
	endfor

	if not Arreq(ver,replicate(1b,nsc)) then $
	message, 'Energy scale inconsistency', /cont

	ord = Scan_order(t_0,t_1,t_2,t_3,t_4,t_5,t_6,t_7)
	otnams = tnams[ord]
	ocnams = cnams[ord]
	com = 'tnext = ' + otnams
	if cfl then com = com + ' & ' + 'cnext = ' + ocnams

	for i = 0, nsc-1 do begin
		dum = execute(com[i])
		if i eq 0 then begin
			tres = tnext
			if cfl then cres = cnext
		endif else begin
			tres = Scan_join(tres,tnext,force=frc)
			if cfl then cres = Scan_join(cres,cnext,ext=1,force=frc)
		endelse
	endfor

	if shofl then begin
		cent = Peak_cent(kdum,err=cer)
		print
		print, cent, cer, form = '("	Center = ",g13.6,"	(",g13.6,")")'
		print
		tit = strcompress('Peak at ' + string(cent,form='(f8.3)') + ' keV, ')
		if keyword_set(ang) then xtit = 'Angle (deg)' $
		else xtit = 'q!dz!n (A!u-1!n)'
		Peak_show, tres, output = oup, png = png, change = chn, $
		tit=tit + 'Counts', xtit = xtit, _extra = _e
		if cfl then begin
			window, 1
			Peak_show, cres, /ynoz, tit = tit + ctit[cwhi], xtit = xtit, $
			_extra = _e
			wset, 0
		endif
	endif

	return, tres
end