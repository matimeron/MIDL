Pro SFRM_to_TIF, snum, fnum, show = sho, count = count

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	ext = '.tif'

	shofl = keyword_set(sho)
	nsc = Scan_list_ver(snum,lis=lis)
	lis = lis[where(lis gt 0 and lis le fildat.nscan,nsc)]
	if nsc gt 0 then begin
		odir = File_get(/dir,title='Select output folder')
		if nsc eq 1 then wfnum = Scan_PD_frames(lis[0],fnum,/ver,/uni) $
		else wfnum = -1
		wha = Scan_field_read(lis,'pdstat')
		val = where(wha eq 5,nval)
		if nval gt 0 then begin
			slis = lis[val]
			count = 0l
			for i = 0, nval-1 do begin
				flis = Scan_pd_frames(slis[i],wfnum,nfram=nfr,/ver,/uni)
				for j = 0, nfr-1 do begin
					img = Scan_pd_read(slis[i],flis[j],/jimg,tit=tit)
					if shofl then Scan_pd_show, img, /raw, tit=tit
					file = odir + Fnamparse(tit) + ext
					write_tiff, file, img, orient=3, /long
				endfor
				count = count + nfr
			endfor
		endif else message, 'No valid APEX data!'
	endif else message, 'Bad or missing scan list!'

	if shofl then begin
		print
		print, string(count,form='(" ",i0," files converted.")')
		print
	endif

	return
end