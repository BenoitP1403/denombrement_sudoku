type partition = {mutable elt :  int array;mutable prof: int array;mutable taille:  int array};;


let init n = 
    let p = {elt = Array.make n 0; prof=Array.make n 0; taille=Array.make n 1 } in
    for k = 0 to n-1 do 
        p.elt.(k) <- k
    done;
p;;

let rec trouver_racine p k = 
    let i=p.elt.(k) in
    if i <> k then
        trouver_racine p i 
    else k;;

let nb_classes p =
    let cpt = ref 0 in
    let n = Array.length p.elt in
    for i = 0 to n-1 do
        if p.elt.(i)=i then
            cpt:= !cpt+1
    done;
    !cpt;;
    
let fusion p i j = 
    let k = trouver_racine p i 
    and l = trouver_racine p j in
    if k <> l then
        let s = p.taille.(k) + p.taille.(l) in 
        p.taille.(k)<-s ;
        p.taille.(l)<-s ;
        if p.prof.(k) < p.prof.(l) then
            p.elt.(k) <- l
        else
            (p.elt.(l) <- k;
            if p.prof.(k) = p.prof.(l) then
                p.prof.(k) <- p.prof.(k) + 1 )

                
let save_to_file_array filename array =
  let oc = open_out_bin filename in
  Marshal.to_channel oc array [];
  close_out oc

let load_from_file_array filename =
  let ic = open_in_bin filename in
  let array = (Marshal.from_channel ic : int array) in
  close_in ic;
  array


let save_partition_to_file filename partition =
  let oc = open_out_bin filename in
  Marshal.to_channel oc partition [];
  close_out oc

let load_partition_from_file filename =
  let ic = open_in_bin filename in
  let partition = (Marshal.from_channel ic : partition) in
  close_in ic;
  partition
                
let ligne_valide boites i =
    let test = Array.make 10 0 in
    for j = 1 to 9 do
        test.(boites.(i).(j)) <- 1+test.(boites.(i).(j))
    done;
    test.(1)<=1 && test.(2)<=1 && test.(3)<=1 && test.(4)<=1 && test.(5)<=1 && test.(6)<=1 && test.(7)<=1 && test.(8)<=1 && test.(9)<=1;;

    
    
let lignes_valides boites = 
    (ligne_valide boites 1) && (ligne_valide boites 2) && (ligne_valide boites 3);;

    
let copy_matrix matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let new_matrix = Array.make_matrix rows cols matrix.(0).(0) in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      new_matrix.(i).(j) <- matrix.(i).(j)
    done;
  done;
  new_matrix;;
    
    
    
let boite_valide boites i =
    let test = Array.make 10 0 in
    for x=1 to 3 do
        for y = (i-1)*3+1 to (i)*3 do 
            test.(boites.(x).(y)) <- test.(boites.(x).(y))+1
        done;
    done;
    test.(1)<=1 && test.(2)<=1 && test.(3)<=1 && test.(4)<=1 && test.(5)<=1 && test.(6)<=1 && test.(7)<=1 && test.(8)<=1 && test.(9)<=1;;

let boites_valides boites = 
    (boite_valide boites 1) && (boite_valide boites 2) && (boite_valide boites 3);;

let rec linf l1 l2 = match l1, l2 with 
    |(_,0::q) -> true
    |(0::q,_) -> true
    |([],[]) -> true
    |(t1::q1, t2::q2) -> (t1=t2 && (linf q1 q2)) || t1<t2;;
    
let print_bool b = 
    if b then print_string "TRUE\n" else print_string "FALSE\n";;

let colonne_valide boites c = 
    (boites.(1).(c)<>boites.(2).(c)
    &&
    boites.(2).(c)<>boites.(3).(c)
    &&
    boites.(3).(c)<>boites.(1).(c)) || boites.(1).(c)=0 || boites.(2).(c)=0 || boites.(3).(c)=0;;

let colonnes_valides boites = 
    let res = ref true in
    for c = 4 to 9 do 
        res := !res && (colonne_valide boites c)
    done;
    !res;;
    
let regle_ordre boites = 
    let l1=[boites.(1).(4);boites.(1).(5);boites.(1).(6);boites.(2).(4);boites.(2).(5);boites.(2).(6);boites.(3).(4);boites.(3).(5);boites.(3).(6)]
    and l2=[boites.(1).(4+3);boites.(1).(5+3);boites.(1).(6+3);boites.(2).(4+3);boites.(2).(5+3);boites.(2).(6+3);boites.(3).(4+3);boites.(3).(5+3);boites.(3).(6+3)] in
    linf l1 l2
    && 
    (boites.(1).(4) <= boites.(1).(5) || boites.(1).(5)=0)
    &&
    (boites.(1).(5) <= boites.(1).(6) || boites.(1).(6)=0)
    &&
    (boites.(1).(7) <= boites.(1).(8) || boites.(1).(8)=0)
    &&
    (boites.(1).(8) <= boites.(1).(9) || boites.(1).(9)=0)


let boites_valide boites = 
    (lignes_valides boites) && (boites_valides boites) &&  (regle_ordre boites) && (colonnes_valides boites);;
    
let afficher_tableau_tableau (tab : int array array) : unit =
  Array.iter (fun sub_tab ->
    Array.iter (fun x -> print_int x; print_string " ") sub_tab;
    print_newline ()
  ) tab


let suivant l c = 
    if c=9 then (l+1,4)
    else (l,c+1)

let disp cpt = if cpt mod 1000 =0 then print_int cpt;;

let generate ()=
    let cpt = ref 0 in
    let resp = Array.make_matrix 4 10 0 in
    resp.(1).(1)<-1;resp.(1).(2)<-2;resp.(1).(3)<-3;resp.(2).(1)<-4;resp.(2).(2)<-5;resp.(2).(3)<-6;resp.(3).(1)<-7;resp.(3).(2)<-8;resp.(3).(3)<-9;
    
    let rec aux x y mat= 
    match (boites_valide mat,x,y) with 
        | (false,_,_) -> []
        | (true,4,4) -> incr cpt;print_int !cpt;print_string "\n";
                        afficher_tableau_tableau mat;print_string "\n";
                        [copy_matrix mat];
        | (true,_,_) -> let res = ref [] in
                        for i=1 to 9 do
                            mat.(x).(y) <- i;
                            let (x',y') = suivant x y in
                                res:= !res@(aux x' y'  (mat));
                            mat.(x).(y)<-0;
                        done;
                        !res;
    in aux 1 4 resp;;

let res1 = generate() ;;
print_string "resultat1:\n";;
print_int(List.length(res1));;


let dep = Array.of_list res1;;
let n = Array.length dep;;
let uf = init n;;

(*On agit sur les classes pour les fusionner*)

let echanger_lignes boites i1 i2 = 
    for j=1 to 9 do
        let tmp = boites.(i1).(j) in
        boites.(i1).(j) <- boites.(i2).(j);
        boites.(i2).(j) <- tmp;
    done;;

let echanger_colonnes boites c1 c2 = 
    for i=1 to 3 do
        let tmp = boites.(i).(c1) in
        boites.(i).(c1) <- boites.(i).(c2);
        boites.(i).(c2) <- tmp;
    done;;

let swap_m m i j i' j' =
    let tmp = m.(i).(j) in
    m.(i).(j) <- m.(i').(j');
    m.(i').(j') <- tmp;;

let echangerb boites x y = 
    for i=1 to 3 do
        for j = 1 to 3 do
            swap_m boites i (j+(y-1)*3) i (j+(x-1)*3)
        done
    done;;

let echangerb23 boites = 
    echangerb boites 2 3;;
let tri_colonnes boites = 
    if (boites.(1).(4)>boites.(1).(5)) then
        echanger_colonnes boites 4 5;
    if (boites.(1).(5)>boites.(1).(6)) then
        echanger_colonnes boites 5 6;
    if (boites.(1).(4)>boites.(1).(5)) then
        echanger_colonnes boites 4 5;
    
    if (boites.(1).(7)>boites.(1).(8)) then
        echanger_colonnes boites 7 8;
    if (boites.(1).(8)>boites.(1).(9)) then
        echanger_colonnes boites 8 9;
    if (boites.(1).(7)>boites.(1).(8)) then
        echanger_colonnes boites 7 8;;
    
let normaliser_chiffres boites = 
    let v1 = boites.(1).(1) and
    v2 = boites.(1).(2) and
    v3 = boites.(1).(3) and
    v4 = boites.(2).(1) and
    v5 = boites.(2).(2) and
    v6 = boites.(2).(3) and
    v7 = boites.(3).(1) and
    v8 = boites.(3).(2) and
    v9 = boites.(3).(3) in
    for i=1 to 3 do
        for j = 1 to 9 do
            match boites.(i).(j) with
                |k when (k=v1) -> (boites.(i).(j) <- 1)
                |k when (k=v2) -> (boites.(i).(j) <- 2)
                |k when (k=v3) -> (boites.(i).(j) <- 3)
                |k when (k=v4) -> (boites.(i).(j) <- 4)
                |k when (k=v5) -> (boites.(i).(j) <- 5)
                |k when (k=v6) -> (boites.(i).(j) <- 6)
                |k when (k=v7) -> (boites.(i).(j) <- 7)
                |k when (k=v8) -> (boites.(i).(j) <- 8)
                |k when (k=v9) -> (boites.(i).(j) <- 9)
        done;
    done ;;

let normaliser boites = 
    normaliser_chiffres boites;
    tri_colonnes boites;
    let l1=[boites.(1).(4);boites.(1).(5);boites.(1).(6);boites.(2).(4);boites.(2).(5);boites.(2).(6);boites.(3).(4);boites.(3).(5);boites.(3).(6)]
    and l2=[boites.(1).(4+3);boites.(1).(5+3);boites.(1).(6+3);boites.(2).(4+3);boites.(2).(5+3);boites.(2).(6+3);boites.(3).(4+3);boites.(3).(5+3);boites.(3).(6+3)] in
    if not (linf l1 l2) then
        echangerb23 boites;
    ;;
    

let indice_boites boites = 
    let r = ref (-1) in
    for i = 0 to n-1 do
        if dep.(i)=boites
            then ( r:=i;  )
    done;
    !r;;

let lier_b boites i = 
    normaliser boites;
    let n'= indice_boites boites in
        match n' with 
            |(-1) -> ()
            | _ -> fusion uf n' i;;




let fusionne1 () = 
    for i = 0 to n-1 do
        print_string "fusionne1 etape :";print_int i;print_string "\n";
        let b' = copy_matrix dep.(i) in
        echanger_lignes b' 1 2;
        lier_b b' i;
        echanger_lignes b' 2 1;
        echanger_lignes b' 1 3;
        lier_b b' i;
        echanger_lignes b' 3 1;
    done;;

    
let exec1() = 
    if Sys.file_exists "fusion1.dat" then 
        let stor= load_partition_from_file "fusion1.dat" in
            uf.elt <- stor.elt;
            uf.prof <- stor.prof;
            uf.taille <- stor.taille;
        else
            fusionne1();;
exec1();;
print_string "res fusionne1: ";;
print_int (nb_classes(uf));;

save_partition_to_file "fusion1.dat" uf;;

(* FUSIONNE1 IS A SUCCESS ON TESTS!  Plus que 6240 classes. Exécution longue (mauvaise optimisation??)*)
(*On pourrait finir en backtracking*)

let fusionne2 () = 
    for i = 0 to n-1 do
        print_string "fusionne2 etape :";print_int i;print_string "\n";
        let b' = copy_matrix dep.(i) in
        echanger_colonnes b' 1 2;
        lier_b b' i;
        echanger_colonnes b' 2 1;
        echanger_colonnes b' 1 3;
        lier_b b' i;
        echanger_colonnes b' 3 1;
    done;;

let exec2() = 
    if Sys.file_exists "fusion2.dat" then 
        let stor= load_partition_from_file "fusion2.dat" in
            uf.elt <- stor.elt;
            uf.prof <- stor.prof;
            uf.taille <- stor.taille;
        else
            fusionne2();;
exec2();;
print_string "res fusionne2: ";;
print_int (nb_classes(uf));;

save_partition_to_file "fusion2.dat" uf;;


(* FUSIONNE2 IS A SUCCESS! Plus que 1089 classes*)


let fusionne3 () = 
    for i = 0 to n-1 do
        print_string "fusionne3 etape :";print_int i;print_string "\n";
        let b' = copy_matrix dep.(i) in
        echangerb b' 1 2;
        lier_b b' i;
        echangerb b' 2 1;
        echangerb b' 1 3;
        lier_b b' i;
        echangerb b' 3 1;
    done;;

let exec3() = 
    if Sys.file_exists "fusion3.dat" then 
        let stor= load_partition_from_file "fusion3.dat" in
            uf.elt <- stor.elt;
            uf.prof <- stor.prof;
            uf.taille <- stor.taille;
        else
            fusionne3();;
exec3();;
print_string "res fusionne3: ";;
print_int (nb_classes(uf));;

save_partition_to_file "fusion3.dat" uf;;




(*On a fini de regrouper les classes, on les regroupe*)
let cree_repr()=
    let nb = nb_classes (uf) in
    let representants = Array.make nb (dep.(0),0)in
    let cpt = ref 0 in
    let somme = ref 0 in
    for i = 0 to n-1 do 
        if uf.elt.(i)=i then
            begin representants.(!cpt)<-(dep.(i),uf.taille.(i));(*print_int uf.taille.(i);print_string "\n"; *)somme := !somme + uf.taille.(i); incr cpt;
            end
    done;
    print_int !somme;
    representants;;
let representants = cree_repr();;

let ordre = [|
            (9,2);(8,2);(7,2);(6,2);(5,2);(4,2);
            (9,3);(8,3);(7,3);(6,3);(5,3);(4,3);
            (9,4);(8,4);(7,4);(6,4);(5,4);(4,4);
            (4,5);(4,6);(4,7);(4,8);(4,9);
            (9,5);(8,5);(7,5);(6,5);(5,5);
            (5,6);(5,7);(5,8);(5,9);
            (9,6);(8,6);(7,6);(6,6);
            (6,7);(6,8);(6,9);
            (9,7);(8,7);(7,7);
            (7,8);(7,9);
            (9,8);(8,8);
            (8,9);
            (9,9)|]

let test_v t =
    t.(1)<=1 && t.(2)<=1 && t.(3)<=1 && t.(4)<=1 && t.(5)<=1 && t.(6)<=1 && t.(7)<=1 && t.(8)<=1 && t.(9)<=1;;

let l_plateau plateau = 
    let res = ref true in
    for i = 1 to 9 do
        let test = Array.make 10 0 in
            for j = 1 to 9 do
                test.(plateau.(i).(j)) <- 1 + test.(plateau.(i).(j))
            done;
        res:= !res && test_v test;
    done;
    !res;;

let c_plateau plateau = 
    let res = ref true in 
    for j = 1 to 9 do
        let test = Array.make 10 0 in
            for i = 1 to 9 do
                test.(plateau.(i).(j)) <- 1 + test.(plateau.(i).(j))
            done;
        res:= !res && test_v test;
    done;
    !res;;

let b_plateau plateau =
    let res = ref true in
    for i = 1 to 3 do
        for j = 1 to 3 do 
            let bigx = (i-1)*3+1 and bigy = (j-1)*3+1 in
                let test = Array.make 10 0 in
                for k = bigx to bigx+2 do
                    for l = bigy to bigy+2  do
                        test.(plateau.(k).(l)) <- 1 + test.(plateau.(k).(l))
                    done
                done;
            res:= !res && test_v test;
        done
    done;
    !res;;
                        


let valide plateau =
    (l_plateau plateau) && (c_plateau plateau) && (b_plateau plateau) ;; 

let nombre_possibilites boites = 
    let plateau = Array.make_matrix 10 10 0 in
    for i=1 to 3 do
        for j =1 to 9 do
            plateau.(i).(j) <- boites.(i).(j)
        done
    done;
    plateau.(4).(1)<-2;
    plateau.(5).(1)<-3;
    plateau.(6).(1)<-5;
    plateau.(7).(1)<-6;
    plateau.(8).(1)<-8;
    plateau.(9).(1)<-9;
    afficher_tableau_tableau plateau;
    let cpt = ref 0 in
    let rec aux k = 
        match k with 
        | m when(m=Array.length ordre) -> if valide plateau then 
                    (incr cpt ;print_int !cpt;print_string "\n";)
        | _ -> if valide plateau then
                        let (x,y)= ordre.(k) in
                        begin
                        for v = 1 to 9 do 
                            plateau.(x).(y) <- v;
                            aux (k+1);
                            plateau.(x).(y) <- 0;
                        done;
                        end
    in aux 0; 
    !cpt;;
print_string "nbres";;
print_int (nombre_possibilites (fst representants.(0)));;
