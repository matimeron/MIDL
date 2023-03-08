Pro Apsu_more

	on_error, 1

;	ef = [3.05,10./3,11./3,4,13./3,14./3,5,16./3,17./3,6,19./3,32./5,20./3,7,$
;	22./3,8,26./3,9,28./3,10,11]
;	ef = reverse(ef)

;	for i = 0l, n_elements(k)-1 do $
;	un_make, k[i], 0.028, !blpar.rgam, 84, 'apsmba_28mm_l*', $
;	!blpar.rsig, !blpar.asig, ang= [0.200,0.100], np= [100,50], /auto, /sho
	
	un_make, 2.282, 0.027, !blpar.rgam, 77, 'apsu_27_s*', at=15, $
	!blpar.rsig, !blpar.asig, ang= [0.04,0.04], np= [40,40], /auto, /sho

	un_make, 2.311, 0.027, !blpar.rgam, 77, 'apsu_27_s*', at=15, $
	!blpar.rsig, !blpar.asig, ang= [0.04,0.04], np= [40,40], /auto, /sho
	
	un_make, 2.459, 0.028, !blpar.rgam, 75, 'apsu_28_s*', at=15, $
	!blpar.rsig, !blpar.asig, ang= [0.04,0.04], np= [40,40], /auto, /sho
	
	un_make, 2.490, 0.028, !blpar.rgam, 75, 'apsu_28_s*', at=15, $
	!blpar.rsig, !blpar.asig, ang= [0.04,0.04], np= [40,40], /auto, /sho

return
end