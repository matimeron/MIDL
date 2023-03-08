Pro PD_diffuse_old, snum, tilt = tau, correct_q = coq, pxval = pxv, qzval = qzv, $
	last= lst, result= res, sig_raw= sig, bac_raw= bac, net_raw= net, _extra =_e

;+
; NAME:
;		PD_DIFFUSE
; VERSION:
;		8.02
; PURPOSE:
;		Reads a list of Pilatus scans, combines separately all signal and
;		background frames and generates net signal, for diffuse scattering.
; CATEGORY:
;		Surface specific
; CALLING SEQUENCE:
;		PD_DIFFUSE( SNUM [, keywords])
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.
;	/CORRECT_Q
;		Switch.  If set, the signal-backgroud subtraction is done for constant
;		Q instead of constant angle.
;	/QZVAL													|	At most one of
;		Switch.  If set, data is displayed (and returned	|	these 2 may be
;		in RESULT) as a function of Q_z.					|	set.  If none
;	/PXVAL													|	is, data is
;		Switch.  If set, data is displayed (and returned 	|	displayed as
;		in RESULT) as a function of detector pixels.		|	function of Q_xy
;	/LAST
;		Switch.  If set, the last data processed is reused.
;	RESULT
;		Optional output, see below.
;	SIG_RAW
;		Optional output, see below.
;	BAC_RAW
;		Optional output, see below.
;	NET_RAW
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;
;		See SCAN_PD_READ for more details.
;
;		Note:	/NORM is set by default in PD_DIFFUSE, it can be disabled by
;				using NORM = 0 in the function call.
; OUTPUTS:
;		Only screen output directly, showing integrated (horizontally) net
;		signal as a function of Q_y.  Other outputs through Opt. Out.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the integrated horizontally net signal as a function of Q_y, in
;		the standard 1D scan format (3 columns).
;	SIG_RAW
;		Returns the sum of all the signal frames (DTH = 0) as a 2D image.
;	BAC_RAW
;		Returns the sum of all the background frames (DTH !=0) as a 2D image.
;	NET_RAW
;		Returns the difference of SIG_RAW and BAC_RAW, as a 2D image.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		PD_DIF_KEEP.  Contains data pertaining to the last processed PD_DIFFUSE
;		data, as follows:
;
;		DEXS	-	Flag, value of 1 indicates that the common block is defined.
;		ALP		-	The Alpha value.
;		SBET	-	The Beta values for all points along the vertical centerline
;		BDTH	-	The background Dth value.
;		LAM		-	The wavelength Lambda.
;		LSIG	-	Last raw signal (2D array).
;		LBAC	-	Last raw background (2D array).
;		DTIT	-	Plot title.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The data must exist and the total number of signal frames must equal
;		this of the background frames.  Also, all the background frames must be
;		taken at same (in absolute value) DTH.
; PROCEDURE:
;		Straightforward, sums separately all the signal (DTH = 0) and background
;		(DTH != 0) frames, integrates the sums horizontally and subtracts,
;		optionally corrected for change of Q.  Reads the data using parameters
;		stored in the FILDAT structure in the common block.  Calls IMG_INT,
;		SCAN_COLUMN, SCAN_LC, SCAN_PD_LCOO, SCAN_PD_CENTER, SCAN_PD_READ,
;		SCAN_SHOW and SPEC_FILE_CHECK.  Calls QVEC from SURF_LIB. Also calls
;		ISNUM, MAKE_GRID, ONE_OF and RANGE_COMP, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2008 by Mati Meron.
;		Modified 5-AUG-2008 by Mati Meron.  Internal changes.  Added keywords
;		PXVAL, QZVAL and LAST.
;		Modified 15-MAY-2008 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 20-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common pd_dif_keep, dexs, alp, sbet, bdth, lam, lsig, lbac, dtit
	on_error, 1
	del = 1e-3

	if keyword_set(lst) then begin
		if Isnum(dexs) then begin
			sig = lsig
			bac = lbac
		endif else message, 'There is no previous data!'
	endif else begin
		Spec_file_check, snum, /pildet, par_const=['In_rot','out_rot'], $
		/close, nsc=nsc, lis=slis, _extra=_e
		sigct = (bacct = (exsig = (exbac = 0l)))
		alp = (fildat.scan[slis[0]].angs)[0]
		bet = (fildat.scan[slis[0]].angs)[1]
		lam = fildat.scan[slis[0]].lambda
		apfl = fildat.scan[slis[0]].pdstat

		for i = 0l, nsc-1 do begin
			dth = Scan_column(slis[i],'Det_th',stat=stat)
			if stat eq 0 then message, 'Invalid scan, aborting!'
			sind = where(abs(dth) lt del,ns,comp=bind,ncomp=nb)
			if ns gt 0 then begin
				tem = Scan_PD_read(slis[i],sind,/jimg,/norm,tit=dtit,_extra=_e)
				if exsig then sig = sig + tem else sig = tem
				exsig = 1l
				sigct = sigct + ns
			endif
			if nb gt 0 then begin
				tem = Scan_PD_read(slis[i],bind,/jimg,/norm,_extra=_e)
				if exbac then bac = bac + tem else bac = tem
				exbac = 1l
				bacct = bacct + nb
				adth = total(abs(dth(bind)))/nb
				if not Isnum(bdth) then bdth = adth
				if abs(bdth - adth) ge del then message,'DTH trouble, aborting!'
			endif
		endfor
		if sigct ne bacct then message, /con, $
		'Signal-background frame number discrepancy, results suspect!'

		if Isnum(tau) then begin
			wtau = !dtor*tau
			cent = Scan_PD_center(snum,dim=dim)
			xz = Make_grid([[0,dim[0]-1],[0,dim[1]-1]],dim)
			xz[0,*,*] = xz[0,*,*] - cent[0]
			xz[1,*,*] = xz[1,*,*] - cent[1]
			wx = reform(xz[0,*,*]*cos(wtau) - xz[1,*,*]*sin(wtau))
			wz = reform(xz[0,*,*]*sin(wtau) + xz[1,*,*]*cos(wtau))
			triangulate, wx, wz, tring
			rx = reform(xz[0,*,0])
			rz = reform(xz[1,0,*])
			sig = trigrid(wx,wz,sig,tring,xout=rx,yout=rz) > 0
			bac = trigrid(wx,wz,bac,tring,xout=rx,yout=rz) > 0
		endif
		lsig = sig
		lbac = bac
		sbet = Scan_PD_lcoo(slis[0],dir='ver',/ang)
		dtit = strmid(dtit,0,strpos(dtit,'_')) +' S# ' + Range_comp(slis)
		dexs = 1
	endelse

	if arg_present(net) then net = sig - bac
	isig = Img_int(sig,/xy_int,emod=apfl,_extra=_e)
	ibac = Img_int(bac,/xy_int,emod=apfl,_extra=_e)
	if keyword_set(coq) then begin
		isig[0,*] = Qvec(alp,sbet,lam=lam,/qtot)
		ibac[0,*] = Qvec(alp,sbet,dth=bdth,lam=lam,/qtot)
	endif

	res = Scan_lc(isig,ibac,coef=[1,-1],/inter)
	case One_of(pxv,qzv) of
		-1	:	begin
					res[0,*] = (Qvec(alp,sbet,lam=lam))[1,*]
					xtit = 'Q!dy!n'
				end
		0	:	begin
					res[0,*] = findgen((size(res))[2])
					xtit = 'Pixel'
				end
		1	:	begin
					res[0,*] = (Qvec(alp,sbet,lam=lam))[2,*]
					xtit = 'Q!dz!n'
				end
	endcase
	Scan_show, res, tit = dtit, xtit = xtit, ytit = 'Counts', _extra = _e

	return
end