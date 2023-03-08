Pro Scan_info, snum, pars = prs, slits = slt, $
	ld_pars = ldp, pd_pars = pdp, ad_pars = adp, full = ful, _extra = _e

;+
; NAME:
;		SCAN_INFO
; VERSION:
;		8.475
; PURPOSE:
;		Displays information about SPEC file or a specific scan within the file.
; CATEGORY:
;		SPEC file processing
; CALLING SEQUENCE:
;		SCAN_INFO, SNUM [, FILNAM] [ keywords]
; INPUTS:
;	SNUM
;		Integer scalar, scan number.  If not given, defaults to 0, in which
;		case general information about the file is displayed.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/PARS
;		Switch.  If set, parameter data is printed.  Not active if /FULL is set.
;	/SLITS
;		Switch.  If set, slit data is printed.  Not active if /FULL is set.
;	/LD_PARS
;		Switch.  If set, Linear detector parameters are printed.  Active only
;		if the scan has linear detector data and /FULL is not set.
;	/PD_PARS
;		Switch.  If set, Pilatus detector parameters are printed.  Active only
;		if the scan has Pilatus detector data and /FULL is not set.
;	/AD_PARS
;		Switch.  If set, APEX detector parameters are printed.  Active only
;		if the scan has APEX detector data and /FULL is not set.
;		
;		Note:	PD_PARS and AD_PARS are synonimous as both will force printing
;				the parameters of the area detector being used, regardless of
;				whether it is Pilatus or APEX.
;	/FULL
;		Switch.  If set, the whole scan is printed to the screen.
;	_EXTRA
;		A formal keyword used to pass keywords to SPEC_FILE_INFO. All
;		SPEC_FILE_INFO keywords are accepted.  Not to be used directly.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		SPEC_FILE.  See SPEC_FILE_INFO for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Reads information pertaining to the scan specified by
;		SNUM, or general information about the file if SNUM is zero or not
;		given, and displays it on the screen.  Calls SCAN_PD_CENTER and
;		SPEC_FILE_INFO.  Also calls DEFAULT, HEAD, RASCLINE and TABULATE from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 5-OCT-2002 by Mati Meron.
;		Modified 25-MAR-2003 by Mati Meron.
;		Modified 5-NOV-2007 by Mati Meron.  Added keywords LD_PARS and PD_PARS.
;		Modified 20-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 20-MAR-2010 by Mati Meron.  Added keyword AD_PARS and internal 
;		changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	fwarn = ['No such file!','','No data in file','File may be corrupted']
	swarn = ['No such scan!','','No data in scan','Scan may be incomplete','',$
			'Data_header - data mismatch','','Illegitimate data present']

	snum = Default(snum,0l,/dtyp) > 0
	Spec_file_info, _extra = _e
	print
	print, 'Filename	:	', fildat.name
	if fildat.status ne 1 then print, fwarn[fildat.status]

	if fildat.status and snum le fildat.nscan then begin
		inf = fildat.scan[snum]
		dum = Rascline(fildat.name,start=inf.ptr[0],buf=inf.lptr[n_s-1]+1,$
			/exact,lin=lin,call=2)
		if snum gt 0 and inf.stat gt 0 then begin
			if inf.stat ne 1 then begin
				print
				print, swarn[inf.stat]
			endif
			if not keyword_set(ful) then begin
				print
				print, 'Scan header	:	', inf.shead
				print, 'Date/time	:	', inf.sdatime
				if inf.scomment ne '' then $
				print, 'Comment	:	',inf.scomment
				print, string(inf.ncr[0],form='(i2)') + ' columns, ' + $
				string(inf.ncr[1],form='(i4)') + ' rows'
				if inf.lindet gt 0 then begin
					print
					print, form = '("Linear detector	: ",i6," channels")', $
					inf.chan
					if not inf.lindet then print, 'Lindet data problems!'
					if keyword_set(ldp) then begin
						print
						tem = strarr(2,3)
						tem[0,*] = ['Sample-detector distance',$
									'Pixel size', $
									'Zero channel'] + ' :'
						tem[1,*] = [string(inf.ldist,form='(f6.1)'),$
									string(inf.lmpc,form='(f5.3)'),$
									string(inf.lns, form='(i4)')]
						head, tem, tit='Linear detector parameters', $
						form=['a32','a8']
					endif
				endif
				if inf.mcstat gt 0 then begin
					print
					print, strtrim(string(inf.mcnum, inf.mchan,$
					form = '(i4," MCA spectra	: ",i6," channels")'),1)
					if not inf.mcstat then print, 'MCA data problems!'
				endif
				if inf.pdstat then begin
					print
					nams = ['','Area','','Pilatus','','APEX','','Camera',$
							'','','','Pilatus1M']
					adnam = nams[inf.pdstat]
					oris = ['horizontal orientation', 'vertical orientation']
					print, adnam + ' detector being used, ' + oris[inf.pdhv]
					print, 'Base PD filename is ' + inf.pdfnam
					if inf.ncr[1] gt 0 then print, strcompress($
					'Frames 0 - '+ string(inf.ncr[1]-1,form='(i4)')+' present')$
					else print, 'No frames present'
					if inf.pdstat eq 1 then print, 'Detector constants missing'
					if inf.pdstat gt 1 and $
					(keyword_set(pdp) or keyword_set(adp)) then begin
						print
						cent = Scan_PD_center(snum)
						scent = '['+ strjoin(string(cent,form='(i4)'),',') +' ]'
						scent = strcompress(scent)
						tem = strarr(2,4)
						tem[0,*] = ['Sample-detector distance',$
									'S4-detector distance', $
									'Pixel size', $
									'Center location'] + ' :'
						tem[1,*] = [string(inf.pdist,form='(f6.1)'),$
									string(inf.pdsdist, form='(f6.1)'),$
									string(inf.pdpix,form='(f5.3)'),$
									strcompress(scent)]
						head, tem, tit= adnam + ' detector parameters', $
						form=['a32','a12']
					endif
				endif
				print
				print, inf.g_l[1:3], form = $
				"('G_L1	:',f9.3,' ;	G_L2	:',f9.3,' ;	G_L3	:',f9.3)"
				print
				print, inf.q_val[1:3], form = $
				'("Q_vector = [",f9.6,", ",f9.6,", ",f9.6,"]")'
				if keyword_set(prs) then begin
					print
					ind = where(inf.pp_set,nset)
					if nset gt 0 then head, fildat.pp_nam[ind] + ' :', $
					inf.pp_val[ind], nr = nset, tit= 'Parameters' $
					else print, 'No parameters'
				endif
				if keyword_set(slt) and (fildat.nsl gt 0) then begin
					print
					ind = where(inf.sl_set,nset)
					if nset gt 0 then begin
						tit = 'Slit Data'
						head = ['Slit','Top','Bottom','Left','Right']
						form = ['a8','f8.4','f8.4','f8.4','f8.4']
						tabulate, fildat.sl_nam[ind], inf.sl_val[0,ind], $
						inf.sl_val[1,ind], inf.sl_val[2,ind],inf.sl_val[3,ind],$
						tit= tit, head= head, form= form
					endif else print, 'No slits'
				endif
				print
				print, 'Peak location	: ', inf.peak_loc, $
				' ;		Peak intensity	: ', inf.peak_I
				print, 'COM = ', inf.com, ' ;	FWHM = ', inf.fwhm, $
				' ;	CEN = ', inf.cen
				if inf.lambda ne 0 then print, inf.lambda, $
				form = '("LAMBDA = ",f6.4)'
			endif else begin
				ddum = '!@#$'
				if inf.lindet then begin
					lin[inf.lptr[3]:inf.lptr[3]+inf.chro-1] = ddum
					lin[inf.lptr[3]] = '"' + string(inf.chro,form='(i6)') + $
					' lines of linear detector data "'
				endif
				if inf.mcstat then begin
					for i = 0l, inf.mcnum-1 do begin
						lin[inf.mclptr[i]:inf.mclptr[i]+inf.mchro-1] = ddum
						lin[inf.mclptr[i]]= '"'+ string(inf.mchro,form='(i6)')+$
						' lines of MCA spectral data "'
					endfor
				endif
				print
				print, lin[where(lin ne ddum)], form = '(a)'
			endelse
		endif else begin
			if snum eq 0 then begin
				if not keyword_set(ful) then begin
					print
					print, 'Date/time	:	', fildat.datime
					if fildat.comment ne '' then $
					print, 'Comment	:	',fildat.comment
					print, string(fildat.nscan,form='(i6)') + ' scans present'
					if fildat.nzp ne 0 then begin
						print
						print, 'Global parameters:'
						for i = 0, fildat.nzp-1 do print, $
						form='(a,"	:	",f)',fildat.zp_nam[i],fildat.zp_val[i]
					endif
					ind = where(fildat.pp_exs,nset)
					if nset gt 0 then begin
						print
						print, 'Listed local parameters:'
						print
						fpp = 0l
						step = 8
						pnam = fildat.pp_nam[ind]
						while fpp lt nset do begin
							lpp = (fpp + step) < nset
							print, strjoin(pnam[fpp:lpp-1],' ')
							fpp = lpp
						endwhile
					endif
				endif else print, lin, form = '(a)'
			endif else print, swarn[inf.stat]
		endelse
	endif else begin
		if fildat.status then begin
			print
			print, 'Highest scan # present in file is ' + $
			string(fildat.nscan,form = '(i4)')
		endif
	endelse

	print
	return
end