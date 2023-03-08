Function Scan_find_old, snum, type = typ, stype = stp, astype = astp, $
	strict = str, show = sho, count = rcnt, null = nul, _extra = _e

;+
; NAME:
;		SCAN_FIND
; VERSION:
;		8.44
; PURPOSE:
;		Locates within a SPEC file groups of scans of specific types.
; CATEGORY:
;		SPEC file processing
; CALLING SEQUENCE:
;		Result = SCAN_FIND ( SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan list.  Either an array of integers or a character scalar or array
;		of any form acceptable by SCAN_LIST_VER.  If the single value 0 is
;		provided, it translates to "all scans within the file".  If no value
;		is provided, SNUM defaults to 0 (i.e. "all scans")
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	TYPE
;		Character scalar, the type of data sought.  Only first 3 letters count.
;		Currently accepted types are:
;
;			Detector	:	Detector linearity scans.  Single.
;			Direct		:	Direct beam scan (H = K = L = 0).  Single.
;			Reflectivity:	Obvious.  Groups.
;			XRF			:	Same as Reflectivity
;			Kscan		:	Obvious.  Groups.
;			GIXOS		:	Obvious.  Groups.
;			Inplane		:	Obvious.  Groups.
;			GID			:	Same as Inplane.
;			Pinhole		:	Standard Pinhole scan(s).  Groups.
;			PGID		:	Pinhole-GID scan(s).  Groups.
;			IH			:	Obvious.  Single.
;			SH			:	Obvious.  Single.
;			OSH			:	Old style SH, involving a2scan on SH and OH.
;
;	STYPE
;		Character scalar representing one of the recognized SPEC scan types.
;		Since SCAN_FIND sets appropriate scan types automatically, STYPE is only
;		needed when one wishes to override the defaults.
;	ASTYPE
;		Same as type, allows to enter another, alternative SPEC scan type.
;	/STRICT
;		Switch.  If set, only strictly valid scans (with STATUS = 1) are
;		recognized.  The default is to recognize all valid scans.
;	/SHOW
;		Switch.  If set the result is printed to the screen (in addition to
;		being returned).
;	COUNT
;		Optional output, see below.
;	/NULL
;		Switch.  If set and COUNT = 0 (i.e. no scans found), the output is set
;		to !NULL.
;	_EXTRA
;		A formal keyword used to pass keywords to SCAN_WHERE.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a character array, where each entry is a compact representation
;		of a list of scans "belonging together".  If nothing is found, returns
;		a null string, or !NULL if /NULL is set.
; OPTIONAL OUTPUT PARAMETERS:
;	COUNT
;		Returns the number of scans/groups found.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Searches for matches within data provided by
;		SCAN_WHERE and calls SCAN_FIELD_READ and SCAN_WHAT when needed.  Also 
;		calls ARREQ, DEFAULT, DIF, FLTROUND, RANGE_COMP, STREQ, STRMATCH_MM and
;		STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-JUL-2006 by Mati Meron.
;		Modified 15-AUG-2006 by Mati Meron.  Added keyword STRICT.
;		Modified 15-FEB-2007 by Mati Meron.  Internal changes.
;		Modified 10-NOV-2007 by Mati Meron.  Added Beta scans.
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-MAY-2010 by Mati Meron.  Complete rewrite, added many new
;		data types.  Added keyword STYPE, removed obsolete keyword LINDET.
;		Modified 25-FEB-2012 by Mati Meron.  Small internal changes.
;		Modified 30-MAY-2014 by Mati Meron.  Added OSH data type.
;		Modified 20-JUN-2015 by Mati Meron.  Internal changes.  Added keywords
;		ASTYPE and NULL.
;		Modified 10-OCT-2015 by Mati Meron.  Internal changes.
;-

	on_error, 1

	zran = 0.005*[-1,1]
	posib = ['Detector','Direct','Reflectivity','XRF','Kscan','GIXOS',$
			'Inplane','GID','Pinhole','PGID','IH','SH','OSH']
	poscod = [0,1,2,2,3,4,5,5,6,7,8,9,10]
	dstp = ['ascan','ascan','hklscan','hklscan','ascan',$
			'ascan','ascan','ascan','ascan','ascan','a2scan'] + '*'
	adstp = ['','','lscan','','','dthscan','dthscan','dthscan','','',''] + '*'
	
	tpos = Strmatch_mm(typ,posib,3,/nosub)
	if tpos ge 0 then tcod = poscod[tpos] $
	else message, 'Missing or unrecognized mode!'
	wstp = Default(stp,dstp[tcod])
	awstp = Default(astp,adstp[tcod])
	wsnum = Default(snum,0)

	sfl = keyword_set(str)
	nores = 1
	res = ['']
	rcnt = 0l

	case tcod of
		0	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor='mabs',$
					/val,strict=str,count=rcnt,_extra=_e)
					if rcnt gt 0 then res = strcompress(string(lis),/rem)
				end
		1	:	begin
					lis = Scan_where(wsnum,stype=wstp,/val,strict=str,$
					aran=zran,bran=zran,dran=zran,pran=2,count=rcnt,_extra=_e)
					if rcnt gt 0 then res = strcompress(string(lis),/rem)
				end					
		2	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor='l',$
					/val,strict=str,dran=zran,count=cnt,det=det,/nul,_extra=_e)
					if awstp ne '' then llis = Scan_where(wsnum,stype=awstp, $
					/val,strict=str,dran=zran,coun=ccnt,det=ddet,/nul,_extra=_e)
					if ccnt gt 0 then begin
						lis = [lis,llis]
						det = [det,ddet]
						s = Sorpurge(lis,net=cnt)
						lis = lis[s]
						det = det[s]
					endif
					if cnt gt 0 then begin
						qzv = fltarr(cnt)
						for i = 0, cnt-1 do begin
							dum = Strparse_mm(det[i],' 	',slis)
							if Streq(slis[2],'hkl',3) then tem = slis[7:8] $
							else tem = slis[3:4] 
							qzv[i] = min(float(tem))
						endfor
						lcnt = cnt
						repeat begin
							dum = where(qzv gt qzv[0],ndum)
							if ndum gt 0 then begin
								ind = dum[0]
								if lcnt gt ind + 1 then begin
									dum = where(qzv[ind+1:*] le qzv[ind],ndum)
									if ndum gt 0 then begin
										jnd = dum[0] + 1 + ind
										tres = Range_comp(lis[0:jnd-1])
										lis = lis[jnd:*]
										qzv = qzv[jnd:*]
										lcnt = lcnt - jnd
									endif else begin
										tres = Range_comp(lis)
										lcnt = 0
									endelse
								endif else begin
									tres = Range_comp(lis)
									lcnt = 0
								endelse
							endif else begin
								tres = Range_comp(lis)
								lcnt = 0
							endelse
							if nores then begin
								res = tres
								nores = 0
							endif else res = [res,tres]
							rcnt = rcnt + 1
						endrep until lcnt eq 0
					endif
				end
		3	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor='k',$
					/val,dran=zran,count=cnt,_extra=_e)
					if cnt gt 0 then begin
						if cnt gt 1 then begin
							dlis = Dif(lis,/lin)
							dum = where(dlis[1:*] gt 1,ndum)
							if ndum gt 0 then j = [0,dum+1,cnt] else j = [0,cnt]
						endif else begin
							ndum = 0
							j = [0,cnt]
						endelse
						for i = 0, ndum do begin
							slis = lis[j[i]:j[i+1]-1]
							if sfl then dum = Scan_what(slis,/val,/str,lis=slis)
							if not Arreq(slis,-1) then begin
								tres = Range_comp(slis)
								if nores then begin
									res = tres
									nores = 0
								endif else res = [res,tres]
								rcnt = rcnt + 1
							endif
						endfor
					endif
				end
		4	:	begin
					lis = Scan_where(wsnum,stype=wstp,/val,/abs,$
					aran=0.05,dran= [0.1,0.5],dth=dth,count=cnt,_extra=_e)
					if cnt gt 0 then begin
						check = total(Scan_field_read(lis,'varan'),2,/pres)
						dum = where(check eq 0, cnt)
						if cnt gt 0 then begin
							lis = lis[dum]
							dth = Fltround(dth[dum],dig=2)
							if cnt gt 1 then begin
								dlis = Dif(lis+dth,/lin)
								dum = where(dlis[1:*] ne 1,ndum)
								if ndum gt 0 then j = [0,dum+1,cnt] $
								else j = [0,cnt]
							endif else begin
								ndum = 0
								j = [0,cnt]
							endelse
							for i = 0, ndum do begin
								slis = lis[j[i]:j[i+1]-1]
								if sfl then dum = $
								Scan_what(slis,/val,/str,lis=slis)
								if not Arreq(slis,-1) then begin
									tres = Range_comp(slis)
									if nores then begin
										res = tres
										nores = 0
									endif else res = [res,tres]
									rcnt = rcnt + 1
								endif
							endfor
						endif
					endif
				end
		5	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor='dth',$
					/val,dran=0.01,count=cnt,det=det,/nul,_extra=_e)
					if awstp ne '' then llis = Scan_where(wsnum,stype=awstp, $
					/val,dran=0.01,count=ccnt,det=ddet,/nul,_extra=_e)
					if ccnt gt 0 then begin
						lis = [lis,llis]
						det = [det,ddet]
						s = Sorpurge(lis,net=cnt)
						lis = lis[s]
						det = det[s]
					endif
					if cnt gt 0 then begin
						sliv = Scan_field_read(lis,'sl_val')
						s4h = total(sliv[*,[16,24]+4],2)
						s5h = total(sliv[*,[16,24]+5],2)
						dum = where(s4h le 4 and s5h le 4,cnt)
						if cnt gt 0 then begin
							lis = lis[dum]
							det = det[dum]
							ddat = fltarr(cnt,3)
							for i = 0, cnt-1 do begin
								dum = Strparse_mm(det[i],' 	',slis)
								if Streq(slis[2],'asc',3) then tem = slis[4:6] $
								else tem = slis[3:5]
								ddat[i,*] = float(tem)
							endfor
							dstep = abs(ddat[*,0] - ddat[*,1])/ddat[*,2]
							dum = where(dstep ge 0.01 and dstep lt 0.25,cnt)
							if cnt gt 0 then begin
								lis = lis[dum]
								if cnt gt 1 then begin
									dlis = Dif(lis,/lin)
									dum = where(dlis[1:*] gt 1,ndum)
									if ndum gt 0 then j = [0,dum+1,cnt]$
									else j = [0,cnt]
								endif else begin
									ndum = 0
									j = [0,cnt]
								endelse
								for i = 0, ndum do begin
									slis = lis[j[i]:j[i+1]-1]
									if sfl then dum = $
									Scan_what(slis,/val,/str,lis=slis)
									if not Arreq(slis,-1) then begin
										tres = Range_comp(slis)
										if nores then begin
											res = tres
											nores = 0
										endif else res = [res,tres]
										rcnt = rcnt + 1
									endif
								endfor
							endif
						endif
					endif
				end
		6	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor='',$
					/val,dran=0.5,count=cnt,det=det,/nul,_extra=_e)
					if awstp ne '' then llis = Scan_where(wsnum,stype=awstp, $
					/val,dran=0.5,count=ccnt,det=ddet,/nul,_extra=_e)
					if ccnt gt 0 then begin
						lis = [lis,llis]
						det = [det,ddet]
						s = Sorpurge(lis,net=cnt)
						lis = lis[s]
						det = det[s]
					endif
					if cnt gt 0 then begin
						sliv = Scan_field_read(lis,'sl_val')
						s4h = total(sliv[*,[16,24]+4],2)
						s5h = total(sliv[*,[16,24]+5],2)
						dum = where(s4h le 4 and s5h gt 8,cnt)
						if cnt gt 0 then begin
							lis = lis[dum]
							det = det[dum]
							ddat = fltarr(cnt,3)
							for i = 0, cnt-1 do begin
								dum = Strparse_mm(det[i],' 	',slis)
								if Streq(slis[2],'asc',3) then tem = slis[4:6] $
								else tem = slis[3:5]
								ddat[i,*] = float(tem)
							endfor
							dstep = abs(ddat[*,0] - ddat[*,1])/ddat[*,2]
							dum = where(dstep lt 0.01 or dstep ge 0.25,cnt)
							if cnt gt 0 then begin
								lis = lis[dum]
								if cnt gt 1 then begin
									dlis = Dif(lis,/lin)
									dum = where(dlis[1:*] gt 1,ndum)
									if ndum gt 0 then j = [0,dum+1,cnt]$
									else j = [0,cnt]
								endif else begin
									ndum = 0
									j = [0,cnt]
								endelse
								for i = 0, ndum do begin
									slis = lis[j[i]:j[i+1]-1]
									if sfl then dum = $
									Scan_what(slis,/val,/str,lis=slis)
									if not Arreq(slis,-1) then begin
										tres = Range_comp(slis)
										if nores then begin
											res = tres
											nores = 0
										endif else res = [res,tres]
										rcnt = rcnt + 1
									endif
								endfor
							endif
						endif
					endif
				end	
		7	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor='dth',$
					/val,dran=0.5,count=cnt,det=det,/nul,_extra=_e)
					if awstp ne '' then llis = Scan_where(wsnum,stype=awstp, $
					/val,dran=0.5,count=ccnt,det=ddet,/nul,_extra=_e)
					if ccnt gt 0 then begin
						lis = [lis,llis]
						det = [det,ddet]
						s = Sorpurge(lis,net=cnt)
						lis = lis[s]
						det = det[s]
					endif
					if cnt gt 0 then begin
						sliv = Scan_field_read(lis,'sl_val')
						s4h = total(sliv[*,[16,24]+4],2)
						s5h = total(sliv[*,[16,24]+5],2)
						dum = where(s4h le 4 and s5h gt 8,cnt)
						if cnt gt 0 then begin
							lis = lis[dum]
							det = det[dum]
							ddat = fltarr(cnt,3)
							for i = 0, cnt-1 do begin
								dum = Strparse_mm(det[i],' 	',slis)
								if Streq(slis[2],'asc',3) then tem = slis[4:6] $
								else tem = slis[3:5]
								ddat[i,*] = float(tem)
							endfor
							dstep = abs(ddat[*,0] - ddat[*,1])/ddat[*,2]
							dum = where(dstep ge 0.01 and dstep lt 0.25,cnt)
							if cnt gt 0 then begin
								lis = lis[dum]
								if cnt gt 1 then begin
									dlis = Dif(lis,/lin)
									dum = where(dlis[1:*] gt 1,ndum)
									if ndum gt 0 then j = [0,dum+1,cnt]$
									else j = [0,cnt]
								endif else begin
									ndum = 0
									j = [0,cnt]
								endelse
								for i = 0, ndum do begin
									slis = lis[j[i]:j[i+1]-1]
									if sfl then dum = $
									Scan_what(slis,/val,/str,lis=slis)
									if not Arreq(slis,-1) then begin
										tres = Range_comp(slis)
										if nores then begin
											res = tres
											nores = 0
										endif else res = [res,tres]
										rcnt = rcnt + 1
									endif
								endfor
							endif
						endif
					endif
				end		
		8	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor='ih',$
					/val,strict=str,dran=zran,count=cnt,det=det,_extra=_e)
					if cnt gt 0 then begin
						rih = fltarr(cnt,2)
						for i = 0, cnt-1 do begin
							dum = Strparse_mm(det[i],' 	',slis)
							rih[i,*] = float(slis[4:5])
						endfor
						rih = Fltround(rih,dig=3)
						dum = where(rih[*,0] ne rih[*,1],rcnt)
						if rcnt gt 0 then $
						res = strcompress(string(lis[dum]),/rem)
					endif
				end
		9	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor=['sh'],$
					/val,strict=str,dran=zran,count=rcnt,_extra=_e)
					if rcnt gt 0 then res = strcompress(string(lis),/rem)
				end
		10	:	begin
					lis = Scan_where(wsnum,stype=wstp,motor=['sh','oh'],$
					/val,strict=str,dran=zran,count=rcnt,_extra=_e)
					if rcnt gt 0 then res = strcompress(string(lis),/rem)
				end
		else:	message, 'Unknown scan type!'
	endcase

	if keyword_set(sho) then begin
		print
		if rcnt gt 0 then print, res, form = '(a)' $
		else print, 'Nothing found!'
	endif
	if keyword_set(nul) and rcnt eq 0 then res = !null

	return, res
end