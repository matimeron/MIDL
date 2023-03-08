Function Scan_LD_sum, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	orientation = ori, chan_range = chr, bin = bin, norm = nrm, $
	angles = ang, signed = sgn, average = ave, list = sls, _extra = _e
;+
; NAME:
;		SCAN_LD_SUM
; VERSION:
;		7.15
; PURPOSE:
;		Summing (or averaging) LD scans.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		SCAN_LD_SUM, SL_0, ..., [,keywords]
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List(s) of linear detector scans, provided in any form that is
;		acceptable (individually) by LD_READ.  The lists will be combined into
;		a single list, internally
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORIENTATION
;		Character string input specifying LD orientation.  Two possible inputs,
;		"horizontal" and "vertical" (first letter suffices).  Default is
;		"vertical".
;	CHAN_RANGE
;		The range of channels to use in the readout.  Default is full range
;		present in the data.
;	BIN
;		Specifies the bin size to be applied to each scan.  Default is 1, i.e.
;		no binning.  Any size provided is always rounded downward to the nearest
;		power of 2 (but the resulting binsize is never smaller than 1).
;		Note:	When both CHAN_RANGE and BIN are present, the CHAN_RANGE values
;				are taken as pre-binned values.
;	/NORM
;		Specifies data normalization.  If NORM is set (with nonzero numeric
;		value) the data is normalized to MONC.  This can be overriden by
;		giving NORM as character value in which case this value specifies
;		that column to be used for normalization.
;	/ANGLES
;		Switch.  If set the data is read as a function of the angles DTHETA
;		(for horizontal) or BETA (for vertical), instead of Q_xy and Q_z.
;	/SIGNED
;		Switch.  If set and the orientation is "horizontal" and /ANGLES is not
;		set, then the Q_xy values are multiplied by SIGN(DTH).  Default is not
;		signed.
;	/AVERAGE
;		Switch.  If set, a weighted (by experimental errors) average is
;		performed instead of a sum.
;	LIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Returns the result in the standard scan form, i.e. a [3,N_POINTS] array
;		where the first column contains the Q_z (or BETA) values for vertical
;		orientation, Q_XY (or DTH) for horizontal one, the second column
;		contains the summed (or averaged) data and the third the data errors.
; OPTIONAL OUTPUT PARAMETERS:
;	LIST
;		Returns the list of all the processed scans as a long integer array.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward summing or averaging.  Calls SCAN_FIELD_READ,
;		SCAN_LD_READ and SCAN_PAR_READ.  Calls LIMG_COO_CONV.  Also calls
;		ARRPACK, DEFAULT, and STRMATCH_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-OCT-2007 by Mati Meron.
;		Modified 15-NOV-2007 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	warn = 'Missing or non-constant '
	cwnam = ['detector distance.','chan/mm.','specular channel.','lambda','G_L']
	dnam = ['Ldist','Lmpc','Lns','Lambda','G_L']
	pnam = ['In_Rot','Out_Rot','Det_Th']
	coset = [1,1,1]
	ptol = 0.005

	wori = abs(Strmatch_mm(Default(ori,'',/dtyp),['hor','ver'],1,/nosub))
	coset[wori+1] = 0
	snum = Arrpack(sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7)
	ldat = Scan_ld_read(snum,norm=nrm,chan_range=chr,bin=bin,list=sls,_extra=_e)
	siz = size(ldat)

	dist = (Scan_field_read(sls,dnam[0],/const,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[0]
	mpc = (Scan_field_read(sls,dnam[1],/const,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[1]
	ns = (Scan_field_read(sls,dnam[2],/const,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[2]
	lam = (Scan_field_read(sls,dnam[3],/const,tol=1e-3,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[3]
	gl3 = Scan_field_read(sls,dnam[4],/const,tol=1.,cfl=cfl)
	if Arreq(cfl,[1,1,1,1]) then gl3 = gl3[3] else message, warn + cwnam[3]

	alp = (Scan_par_read(sls,pnam[0],const=coset[0],tol=ptol,cfl=cfl))[0]
	if not cfl then message, 'Alpha not constant!'
	bet = Scan_par_read(sls,pnam[1],const=coset[1],tol=ptol,cfl=cfl)
	if not cfl then message, 'Not a vertical orientation scan!'
	if not wori then begin
		bet = total(bet)/siz[1]
		dth = Scan_par_read(sls,pnam[2],const=coset[2],tol=ptol,cfl=cfl)
		if cfl then dth = dth[0] $
		else message, 'Not a horizontal orientation scan!'
	endif else bet = bet[0]

	n = bin*lindgen(siz[2]) + (bin-1)/2. + (Default(chr,0l,/dtyp))[0]
	wdist = dist + gl3*(1/cos(!dtor*bet) - 1)
	qvl = 1 - keyword_set(ang)
	res = fltarr(3,siz[2])

	res[0,*] = Limg_coo_conv(n,dist=wdist,ipix=mpc,cen=ns,$
	alp=alp,bet=bet,dth=dth,lam=lam,dir=wori,qval=qvl,/xrev,sign=sgn)

	if keyword_set(ave) then begin
		wei = 1/total(ldat[*,*,1]^2,2)
		wei = wei/total(wei)
	endif else wei = replicate(1.,siz[1])

	for i = 0l, siz[1] - 1 do begin
		res[1,*] = res[1,*] + wei[i]*ldat[i,*,0]
		res[2,*] = res[2,*] + (wei[i]*ldat[i,*,1])^2
	endfor
	res[2,*] = sqrt(res[2,*])

	return, res
end