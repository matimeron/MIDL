Pro LD_zbin, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	range = ran, zbin = zbn, offset = off, zpositive = zpo, title = tit, $
	log = log, angles = ang, _extra =_e

;+
; NAME:
;		LD_ZBIN
; VERSION:
;		5.6
; PURPOSE:
;		Display of binned linear detector data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		LD_ZBIN, SL_0, ..., [,keywords]
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List of linear detector scans, provided in any form that is acceptable
;		by LD_READ.  If more than one input is provided, LD_READ will
;		attempt patching in the horizontal direction.
;
;		Alternatively, SL_O may already contain linear detector data in the form
;		returned by LD_READ.  In such case it is used as is and any additional
;		inputs (if any) are ignored.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RANGE
;		A two element vector specifying Qz region of interest in [min,max]
;		order.  Optionally, a scalar specifying the lower limit of the ROI (with
;		the higher limit provided by the data.  If not given, the region is
;		determined by the Qz values present in the data.
;	ZBIN
;		The size of the bin, in the Qz direction, to be used.  Default is 64.
;		Note that, unlike the situation in LD_READ where the binsize is limited
;		to powers of 2, here binsize is arbitrary.
;
;		Note:   If ZBIN is provided as a non-integer type, it is taken as
;				binsize in units of Q_z (or angle, if /ANGLES is set), not as
;				a number of channels which is the default
;	OFFSET
;		An offset parameter (default value is 1) used to generate the plotting
;		offset between consecutive data slices.  In the case of linear plot, the
;		offset used is OFFSET*MAX(data).  In the case of a logarithmic plot,
;		the ratio of consecutive slices is (1 + 1/OFFSET).
;	/ZPOSITIVE
;		Switch.  If set, only Qz >= 0 values are included in the calculation.
;		This means that the lower limit of the Qz region of interest will be
;		the greater of this provided by RANGE (if any) and 0.
;	TITLE
;		Optional title to be used in the graphics output.  If not provided, a
;		title is generated internally.
;	/LOG
;		Switch.  Specifies a logarithmic plot.
;	/ANGLES
;		Switch.  If set the data is read and displayed as a function of the
;		angles DTHETA (for horizontal) and BETA (for vertical), instead of Q_xy
;		and Q_z.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		None other than the graphic output.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, rebinning.  Calls LD_READ.  Also calls DEFAULT, ISNUM,
;		LABELS and WHERINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2006 by Mati Meron.
;		Modified 20-JUN-2006 by Mati Meron.  Added keywords /ANGLES and $
;		ZPOSITIVE and changed default value of BINSIZE to 64.
;		Modified 10-NOV-2006 by Mati Meron.  Changed keyword BINSIZE to ZBIN
;		and added option of specifying ZBIN in units of Q_z (or angle).
;-

	on_error, 1

	if keyword_set(ang) then begin
		xtit = 'Dth'
		ytit = 'Beta'
	endif else begin
		xtit = 'Q!dxy!n'
		ytit = 'Q!dz!n'
	endelse

	siz = size(sl_0)
	if siz[0] eq 3 then begin
		wdat = sl_0
		tit = Default(tit,'')
	endif else begin
		wdat = LD_read( sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
		angle = ang, title = tit, _extra = _e)
		siz = size(wdat)
	endelse
	dum = (Wherinstruct('norm',_e))[0]
	if dum ge 0 then _e.(dum) = 0

	qxy = reform(wdat[0,*,0])
	qz = reform(wdat[1,0,*])
	qzran = [min(qz,max=max),max]
	case n_elements(ran) of
		0	:	wran = qzran
		1	:	wran = [ran[0],qzran[1]]
		2	:	wran = 1.*ran
		else:	wran = 1.*ran[0:1]
	endcase
	wran = qzran[0] > wran[sort(wran)] < qzran[1]
	if keyword_set(zpo) then wran = wran > 0

	wzbn = Default(zbn,64l)
	if not Isnum(wzbn,/int) then begin
		nsli = floor((wran[1] - wran[0])/wzbn) > 1
		dum = where(qz ge wran[0] and qz le (wran[0] + nsli*wzbn), ndum)
		wzbn = round(ndum/nsli)
	endif
	ind = where(qz ge wran[0] and qz le wran[1],nind)
	if nind gt 0 then begin
		wzbn = wzbn < nind
		nsli = nind/wzbn
	endif else message, 'Null range, cannot proceed!'

	rdat = rebin(reform(wdat[2,*,ind[0]:ind[0]+nsli*wzbn-1]),siz[2],nsli)
	qzval = rebin(qz[ind[0]:ind[0]+nsli*wzbn-1],nsli)
	if nsli gt 1 then delqz = (qzval[nsli-1] - qzval[0])/(nsli-1) $
	else delqz = nind*(qz[siz[3]-1] - qz[0])/(siz[3]-1)
	fir = string(qzval - delqz/2,form='(f6.3)')
	sec = string(qzval + delqz/2,form='(f6.3)')
	lab = strcompress(fir + '<' + ytit + '<' + sec,/rem)

	woff = Default(off,1.,/dtyp)
	poff = woff*max(rdat)
	rat = 1 + 1/woff
	zer = 0*qxy
	if keyword_set(log) then begin
		plot, qxy, qxy, /ylog, yrange=poff*[1,rat^nsli], $
		ytickformat= 'Scinot', xstyle = 1, ystyle = 1, tit = tit, xtit = xtit, $
		/nodata, _extra = _e
		for i = 0l, nsli - 1 do begin
			oplot, qxy, poff*rat^i + zer, line = 1
			oplot, qxy, (rat^i)*(rdat[*,i] + poff)
			Labels, max(qxy), poff*rat^(indgen(nsli) + 0.6), lab, align = 1.1, $
			_extra = _e
		endfor
	endif else begin
		plot, qxy, qxy, yrange=[0,poff*(nsli-1)+ (max(rdat[*,nsli-1])> poff)],$
		ytickformat ='Scinot', xstyle = 1, ystyle=1, tit = tit, xtit = xtit, $
		/nodata, _extra = _e
		for i = 0l, nsli - 1 do begin
			oplot, qxy, i*poff + zer, line = 1
			oplot, qxy, i*poff + rdat[*,i]
		endfor
		Labels, max(qxy), poff*(indgen(nsli)+ 0.6), lab, align= 1.1, _extra= _e
	endelse

	return
end