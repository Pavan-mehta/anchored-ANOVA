
program anova


! The present code is for c-anova in 3 dimension. However extenstion in N dimension is easy.
! The trucation dimension is 2.
! Author: Pavan Mehta, AMIMechE
! Reseatch Engineer, M2P2 lab, CNRS UMR 7340, France
! Email: mehtapavanp@gmail.com

implicit none

integer :: i,j,k, anc1, anc2, anc3, N1, N2, N3

parameter (N1=10)
parameter (N2=10)
parameter (N3=10)

double precision F(N1,N2,N3), F_1(N1), F_2(N2), F_3(N3), F_12(N1,N2) 
double precision F_13(N1,N3), F_23(N2,N3), Error(N1,N2,N3), f_0, F_anova(N1,N1,N3)


write(*,*) "Author: Pavan Mehta"
write(*,*) "Research Engineer - M2P2 Lab"
write(*,*) "Email: mehtapavanp@gmail.com"


!Setting up the Anchors


!anchor in first dimesnion = anc1
!anchor in second dimesnion = anc2
!anchor in third dimesnion = anc3

anc1 = N1/2 ! N1 is even, similarly for N2 and N3
anc2 = N2/2
anc3 = N3/2


!**********************************************************
! Test Function evaluation
!**********************************************************


Do i=1, N1

	Do j=1, N2

		Do k=1, N3

			F(i,j,k) =  i**2 + j**2 + k**2

		enddo
	enddo
enddo





!**********************************************************
! c-Anova Decomposition
!**********************************************************


! Mean or zeroth evaluation

f_0 = F(anc1,anc2,anc3)


write(*,*) "Ancored Anova Depcompisition"
write(*,*) "Anchored Points", anc1, anc2, anc3


!Computing first ans Second order term. Simultaiounly preforming c-Anova



Do i=1, N1

	Do j=1, N2

		Do k=1, N3


			!-----------------------------------------------------------------------------------

			!First order Terms			


			F_1(i) = F(i,anc2,anc3) - f_0

			F_2(j) = F(anc1,j,anc3) - f_0

			F_3(k) = F(anc1,anc2,k) - f_0

			
			!-----------------------------------------------------------------------------------
			
			!Second order Terms


			F_12(i,j) = F(i,j,anc3) - F_1(i) - F_2(j) - f_0
			
			F_13(i,k) = F(i,anc2,k) - F_1(i) - F_3(k) - f_0

			F_23(j,k) = F(anc1,j,k) - F_2(j) - F_3(k) - f_0


			!-----------------------------------------------------------------------------------
			
			!Function evaluation using c-Anova


			F_anova(i,j,k) = f_0 + F_1(i) + F_2(j) + F_3(k) + F_12(i,j) + F_13(i,k) + F_23(j,k)


			Error(i,j,k) = F_anova(i,j,k) - F(i,j,k)

			
			write(*,*) i,j,k, "Error", Error(i,j,k), "=", F_anova(i,j,k), "-", F(i,j,k)


		enddo
	enddo
enddo
    
    

end program anova
