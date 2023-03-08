Pro PD_GID_zbin, snum, verify = ver, range = ran, zbin = zbn, offset = off, $
	zpositive= zpo, title= tit, log= log, raw= raw, angles= ang, labcolor= lco,$
	_extra =_e

;+
; NAME:
;		PD_GID_ZBIN
; VERSION:
;		7.13
; PURPOSE:
;		Display of binned patched PD data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GID_ZBIN, SNUM... [,keywords]
; INPUTS:
;		A list of scan numbers, in any form recognizable by RANGE_PROC.  If more
;		than one scan is provided, PD_GID_READ will attempt patching in the
;		horizontal direction.
;		Alternatively, SNUM may already contain PD data in the form returned by
;		PD_GID_READ.  In such case it is used as is.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	RANGE
;		A two element vector specifying Qz region of interest in [min,max]
;		order.  Optionally, a scalar specifying the lower limit of the ROI (with
;		the higher limit provided by the data.  If not given, the region is
;		determined by the Qz values present in the data.
;	ZBIN
;		The size of the bin, in the Qz direction, to be used.  Default is 64.
;		Note that the binsize is arbitrary, not limited to powers of 2.
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
;	LABCOLOR
;		integer scalar, specifies label color.  Default is black.
;	/LOG
;		Switch.  Specifies a logarithmic plot.
;	/RAW
;		Switch.  If set, the coordinates of the image are "frame #"	|
;		forhorizontal and "pixel #" for vertical, instead of Q_xy	|
;		and Q_z.  Note that if multiple scans are patched, the frame|	Only one
;		number listed does not correspond to a frame number in any	|	of these
;		specific scan.												|	two may
;	/ANGLES															|	be used.
;		Switch.  If set the coordinates of the image are the angles	|
;		DTHETA (for horizontal) and BETA for vertical, instead of	|
;		Q_xy and Q_z.												|
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
;
;		Note:	There is no need to pass /NORM to embedded routines since it is
;				already set by default in PD_GID_READ.  It can be disabled by
;				using NORM = 0 in the function call.
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
;		Straightforward, rebinning.  Calls PD_GID_READ.  Also calls DEFAULT,
;		FPU_FIX, ISNUM, LABELS, ONE_OF and WHERINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2007 by Mati Meron as a minor modification (changes only
;		in the input part) of LD_ZBIN.
;		Modified 10-MAY-2008 by Mati Meron.  Internal and input changes.  Added
;		keyword /RAW.
;		Modified 15-MAY-2008 by Mati Meron.  Added keyword LABCOLOR.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;-

	on_error, 1

	typ = 1 - One_of(ang,raw)
	xytit = [['frame','Dth','Q!dxy!n'],['pix','Beta','Q!dz!n']]
	xtit = xytit[typ,0]
	ytit = xytit[typ,1]

	if (size(snum))[0] eq 3 then wdat = snum else $
	wdat = PD_GID_read(snum,veri=ver,raw=raw,angles=ang,title= dtit,_extra=_e)
	siz = size(wdat)
	if siz[0] ne 3 then message, 'Invalid Data!'
	tit = Default(tit,Default(dtit,''))
	wlco = Default(lco,!p.color)
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

	wzbn = Default(zbn,64l > siz[3]/8)
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
	if typ eq 0 then begin
		fir = string(ceil(qzval - delqz/2),form='(i0)')
		sec = string(ceil(qzval + delqz/2),form='(i0)')
	endif else begin
		fir = string(qzval - delqz/2,form='(f6.3)')
		sec = string(qzval + delqz/2,form='(f6.3)')
	endelse
	lab = strcompress(fir + '!9l!x' + ytit + '<' + sec,/rem)

	woff = Default(off,1.,/dtyp)
	poff = woff*max(rdat)
	rat = 1 + 1/woff
	zer = 0*qxy
	if keyword_set(log) then begin
		plot, qxy, qxy, /ylog, yrange=poff*[1,rat^nsli], /nodata, $
		xstyle = 1, ystyle = 1, tit = tit, xtit = xtit, _extra = _e
		dum = (Wherinstruct('dev',_e))[0]
		if dum ge 0 then _e.(dum) = 0
		for i = 0l, nsli - 1 do begin
			oplot, qxy, poff*rat^i + zer, line = 1
			oplot, qxy, (rat^i)*(rdat[*,i] + poff)
			Labels, max(qxy), poff*rat^(indgen(nsli) + 0.6), lab, align = 1.1, $
			color = wlco, _extra = _e
		endfor
	endif else begin
		plot, qxy, qxy, yrange=[0,poff*(nsli-1)+ (max(rdat[*,nsli-1])> poff)],$
		/nodata, xstyle = 1, ystyle = 1, tit = tit, xtit = xtit, _extra = _e
		dum = (Wherinstruct('dev',_e))[0]
		if dum ge 0 then _e.(dum) = 0
		for i = 0l, nsli - 1 do begin
			oplot, qxy, i*poff + zer, line = 1
			oplot, qxy, i*poff + rdat[*,i]
		endfor
		Labels, max(qxy), poff*(indgen(nsli)+ 0.6), lab, align= 1.1, $
		color = wlco, _extra =_e
	endelse

	dum = FPU_fix(0)
	return
end