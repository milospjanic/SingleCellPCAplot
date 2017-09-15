# SingleCellPCAplot

SingleCellPCAplot is an R script that performs principal component analysis of single cell RNAseq data and outputs a PCA plot with dots colored according to the expression of a gene of interest presented in a gradiant scale. SingleCellPCAplot takes the mastertable with RPKM values from a single cell RNAseq experiment and it also requires an input parameter, i.e. a gene of interest. The expression of that gene will be presented as a color gradient. The script will perform principal component analysis and output a scatter plot of PC1/PC2 and PC2/PC3.

#Example run

Input file with RPKM values showld be tab separated with the gene name in the first column and cell labels in the first row. For example:

<pre>
Activated.Postn..CD31..CD45.	Activated.Postn..CD31..CD45..1	Activated.Postn..CD31..CD45..2	Activated.Postn..CD31..CD45..3	Activated.Postn..CD31..CD45..4	Activated.Postn..CD31..CD45..5	Activated.Postn..CD31..CD45..6	Activated.Postn..CD31..CD45..7	Activated.Postn..CD31..CD45..8	Activated.Postn..CD31..CD45..9	Activated.Postn..CD31..CD45..10	Activated.Postn..CD31..CD45..11	Activated.Postn..CD31..CD45..12	Activated.Postn..CD31..CD45..13	Activated.Postn..CD31..CD45..14	Activated.Postn..CD31..CD45..15	Activated.Postn..CD31..CD45..16	Activated.Postn..CD31..CD45..17	Activated.Postn..CD31..CD45..18	Activated.Postn..CD31..CD45..19	Activated.Postn..CD31..CD45..20	Activated.Postn..CD31..CD45..21	Activated.Postn..CD31..CD45..22	Activated.Postn..CD31..CD45..23	Activated.Postn.traced.Postn..CD31..CD45.	Activated.Postn.traced.Postn..CD31..CD45..1	Activated.Postn.traced.Postn..CD31..CD45..2	Activated.Postn.traced.Postn..CD31..CD45..3	Activated.Postn.traced.Postn..CD31..CD45..4	Activated.Postn.traced.Postn..CD31..CD45..5	Activated.Postn.traced.Postn..CD31..CD45..6	Activated.Postn.traced.Postn..CD31..CD45..7	Activated.Postn.traced.Postn..CD31..CD45..8	Activated.Postn.traced.Postn..CD31..CD45..9	Activated.Postn.traced.Postn..CD31..CD45..10	Activated.Postn.traced.Postn..CD31..CD45..11	Activated.Postn.traced.Postn..CD31..CD45..12	Activated.Postn.traced.Postn..CD31..CD45..13	Activated.Postn.traced.Postn..CD31..CD45..14	Activated.Postn.traced.Postn..CD31..CD45..15	Activated.Postn.traced.Postn..CD31..CD45..16	Activated.Postn.traced.Postn..CD31..CD45..17	Activated.Postn.traced.Postn..CD31..CD45..18	Activated.Postn.traced.Postn..CD31..CD45..19	Activated.Postn.traced.Postn..CD31..CD45..20	Activated.Postn.traced.Postn..CD31..CD45..21	Activated.Postn.traced.Postn..CD31..CD45..22	Activated.Postn.traced.Postn..CD31..CD45..23	Activated.Postn.traced.Postn..CD31..CD45..24	Activated.Postn.traced.Postn..CD31..CD45..25	Activated.Postn.traced.Postn..CD31..CD45..26	Activated.Postn..CD31..CD45..24	Activated.Postn..CD31..CD45..25	Activated.Postn..CD31..CD45..26	Activated.Postn..CD31..CD45..27	Activated.Postn..CD31..CD45..28	Activated.Postn..CD31..CD45..29	Activated.Postn..CD31..CD45..30	Activated.Postn..CD31..CD45..31	Activated.Postn..CD31..CD45..32	Activated.Postn..CD31..CD45..33	Activated.Postn..CD31..CD45..34	Activated.Postn..CD31..CD45..35	Activated.Postn..CD31..CD45..36	Activated.Postn..CD31..CD45..37	Activated.Postn..CD31..CD45..38	Activated.Postn..CD31..CD45..39	Activated.Postn..CD31..CD45..40	Activated.Postn..CD31..CD45..41	Activated.Postn..CD31..CD45..42	Activated.Postn..CD31..CD45..43	Activated.Postn..CD31..CD45..44	Activated.Postn..CD31..CD45..45	Activated.Postn..CD31..CD45..46	Activated.Postn..CD31..CD45..47	Activated.Postn..CD31..CD45..48	Activated.Postn..CD31..CD45..49	Activated.Postn..CD31..CD45..50	Activated.Postn..CD31..CD45..51	Activated.Tcf21.Traced..Act..TCF21.	Activated.Tcf21.Traced..Act..TCF21..1	Activated.Tcf21.Traced..Act..TCF21..2	Activated.Tcf21.Traced..Act..TCF21..3	Activated.Tcf21.Traced..Act..TCF21..4	Activated.Tcf21.Traced..Act..TCF21..5	Activated.Tcf21.Traced..Act..TCF21..6	Activated.Tcf21.Traced..Act..TCF21..7	Activated.Tcf21.Traced..Act..TCF21..8	Activated.Tcf21.Traced..Act..TCF21..9	Activated.Tcf21.Traced..Act..TCF21..10	Activated.Tcf21.Traced..Act..TCF21..11	Activated.Tcf21.Traced..Act..TCF21..12	Activated.Tcf21.Traced..Act..TCF21..13	Activated.Tcf21.Traced..Act..TCF21..14	Activated.Tcf21.Traced..Act..TCF21..15	Activated.Tcf21.Traced..Act..TCF21..16	Activated.Tcf21.Traced..Act..TCF21..17	Activated.Tcf21.Traced..Act..TCF21..18	Activated.Tcf21.Traced..Act..TCF21..19	Activated.Tcf21.Traced..Act..TCF21..20	Activated.Tcf21.Traced..Act..TCF21..21	Activated.Tcf21.Traced..Act..TCF21..22	Activated.Tcf21.Traced..Act..TCF21..23	Activated.Tcf21.Traced..Act..TCF21..24	Activated.Tcf21.Traced..Act..TCF21..25	Activated.Tcf21.Traced..Act..TCF21..26	Activated.Tcf21.Traced..Act..TCF21..27	Activated.Tcf21.Traced..Act..TCF21..28	Activated.Tcf21.Traced..Act..TCF21..29	Activated.Tcf21.Traced..Act..TCF21..30	Activated.Tcf21.Traced..Act..TCF21..31	Activated.Tcf21.Traced..Act..TCF21..32	Activated.Tcf21.Traced..Act..TCF21..33	Activated.Tcf21.Traced..Act..TCF21..34	Activated.Tcf21.Traced..Act..TCF21..35	Activated.Tcf21.Traced..Act..TCF21..36	Activated.Tcf21.Traced..Act..TCF21..37	Activated.Tcf21.Traced..Act..TCF21..38	Activated.Tcf21.Traced..Act..TCF21..39	Activated.Tcf21.Traced..Act..TCF21..40	Activated.Tcf21.Traced..Act..TCF21..41	Activated.Tcf21.Traced..Act..TCF21..42	Activated.Tcf21.Traced..Act..TCF21..43	Postn..resident.CD31..CD45.	Postn..Resident.Tcf21.	Postn..Resident.Tcf21..1	Postn..Resident.Tcf21..2	Resident.Postn..CD31..CD45.	Resident.Postn..CD31..CD45..1	Resident.Postn..CD31..CD45..2	Resident.Postn..CD31..CD45..3	Resident.Postn..CD31..CD45..4	Resident.Postn..CD31..CD45..5	Resident.Postn..CD31..CD45..6	Resident.Postn..CD31..CD45..7	Resident.Postn..CD31..CD45..8	Resident.Postn..CD31..CD45..9	Resident.Postn..CD31..CD45..10	Resident.Postn..CD31..CD45..11	Resident.Tcf21traced.Tcf21...uninjured.	Resident.Tcf21traced.Tcf21...uninjured..1	Resident.Tcf21traced.Tcf21...uninjured..2	Resident.Tcf21traced.Tcf21...uninjured..3	Resident.Tcf21traced.Tcf21...uninjured..4	Resident.Tcf21traced.Tcf21...uninjured..5	Resident.Tcf21traced.Tcf21...uninjured..6	Resident.Tcf21traced.Tcf21...uninjured..7	Resident.Tcf21traced.Tcf21...uninjured..8	Resident.Tcf21traced.Tcf21...uninjured..9	Resident.Tcf21traced.Tcf21...uninjured..10	Resident.Tcf21traced.Tcf21...uninjured..11	Resident.Tcf21traced.Tcf21...uninjured..12	Resident.Tcf21traced.Tcf21...uninjured..13	Resident.Tcf21traced.Tcf21...uninjured..14	Resident.Tcf21traced.Tcf21...uninjured..15	Resident.Tcf21traced.Tcf21...uninjured..16	Untraced.Activated.CD31..CD45..cells.from.scar	Untraced.Activated.CD31..CD45..cells.from.scar.1	Untraced.Activated.CD31..CD45..cells.from.scar.2	Untraced.Activated.CD31..CD45..cells.from.scar.3	Untraced.Activated.CD31..CD45..cells.from.scar.4	Untraced.Activated.CD31..CD45..cells.from.scar.5	Untraced.Activated.CD31..CD45..cells.from.scar.6	Untraced.Activated.CD31..CD45..cells.from.scar.7	Untraced.Activated.CD31..CD45..cells.from.scar.8	Untraced.Activated.CD31..CD45..cells.from.scar.9	Untraced.Activated.CD31..CD45..cells.from.scar.10	Untraced.Activated.CD31..CD45..cells.from.scar.11	Untraced.Activated.CD31..CD45..cells.from.scar.12	Untraced.Activated.CD31..CD45..cells.from.scar.13	Untraced.Activated.CD31..CD45..cells.from.scar.14	Untraced.Activated.CD31..CD45..cells.from.scar.15	Untraced.Activated.CD31..CD45..cells.from.scar.16	Untraced.Activated.CD31..CD45..cells.from.scar.17	Untraced.Activated.CD31..CD45..cells.from.scar.18	Untraced.Activated.CD31..CD45..cells.from.scar.19	Untraced.Activated.CD31..CD45..cells.from.scar.20	Untraced.Activated.CD31..CD45..cells.from.scar.21	Untraced.Activated.CD31..CD45..cells.from.scar.22	Untraced.Activated.CD31..CD45..cells.from.scar.23	Untraced.Activated.CD31..CD45..cells.from.scar.24	Untraced.Activated.CD31..CD45..cells.from.scar.25	Untraced.Activated.CD31..CD45..cells.from.scar.26	Untraced.Activated.CD31..CD45..cells.from.scar.27	Untraced.Activated.CD31..CD45..cells.from.scar.28
Ubb	13.86207	11.982969	13.541125	13.697253	13.33165	13.840584	13.246747	13.458857	13.26546	13.684786	13.597669	13.25915	13.152725	13.771079	13.655919	13.688052	13.673941	13.222902	13.10571	13.851502	14.036093	12.124432	12.97331	13.603688	12.130227	12.993758	12.805208	13.596723	13.071537	12.937836	13.98242	13.699752	13.707857	13.578852	13.505114	13.285303	13.326573	13.536225	13.640275	12.871668	13.378158	13.585896	13.033744	13.583216	13.078607	13.21797	13.644508	13.556689	13.204784	13.555723	13.006861	13.55294	12.949418	13.370037	13.793067	9.233603	13.200138	13.255721	12.728471	9.693918	12.854245	12.576226	13.171809	13.466875	13.680834	13.874989	12.581031	13.037635	13.23231	13.139296	13.203805	12.278761	13.086028	13.445443	7.3041515	13.13601	11.70386	13.086179	13.022582	12.917979	13.348916	14.11031	11.590682	12.868178	13.300514	13.205054	13.375191	12.641065	13.061613	13.38733	13.343003	13.436065	13.284553	12.941179	12.736605	13.115344	13.356303	13.439159	13.124816	13.415038	12.986815	13.74437	12.891072	13.602805	13.569527	13.231581	13.251858	13.6313	13.741398	12.44298	11.957142	13.071979	11.395087	12.756274	13.4373865	12.89822	13.741532	13.614871	13.571794	13.96849	13.524195	12.699591	13.17079	13.668595	13.242242	13.063265	13.443845	13.589277	13.39259	13.62244	13.60457	13.332902	13.095536	12.718382	13.576431	13.310084	13.65144	12.302218	13.468868	13.5846405	13.28735	13.590961	13.085101	13.886997	13.858532	13.38972	13.770371	13.325242	13.4441805	12.087826	13.501807	13.628097	13.243867	13.3985195	13.82952	13.808521	13.06711	13.339168	13.220488	13.629195	13.62505	12.894407	13.045815	12.779445	12.821273	13.861576	13.54156	13.487502	13.459126	12.795243	13.164552	13.539584	12.541845	7.194168	13.108057	13.442842	13.012995	13.543457	12.922864	13.480866	13.445474	11.189922	13.875786	12.85153	14.007434
Dcn	13.882132	13.075001	14.576301	13.590265	14.100907	13.617802	12.681775	14.073322	14.367706	14.354165	13.274112	13.986761	14.302691	13.697014	13.959587	13.649247	13.924157	14.398769	14.060152	14.411955	13.817853	14.451709	12.022597	14.225913	12.148491	12.691384	12.976594	12.659506	11.718438	11.444596	14.186649	11.208649	13.562921	13.366921	12.928359	12.665302	13.542493	13.352703	13.769693	13.2039	12.111941	13.437403	13.476092	14.346797	13.253824	12.882376	13.832033	12.526004	13.55148	13.147661	12.985948	14.457925	13.991306	12.817828	4.064269	11.893489	14.1654215	13.433713	11.694662	5.7535625	12.192932	12.725759	11.582806	11.791403	13.1293955	13.002272	12.97249	13.8485365	12.997706	12.1680765	11.797325	12.107612	13.417193	13.602919	11.230904	13.448433	13.619003	8.01185	13.0395565	12.491058	13.935414	13.617269	12.438251	14.006187	13.744714	12.720655	13.803087	8.021097	11.595772	13.450832	14.346381	13.242195	10.633342	11.9391985	12.953378	12.207495	13.145295	13.229276	13.035693	14.177472	13.318966	13.266651	12.262985	12.983824	14.491799	13.88723	12.887334	12.390604	12.747575	12.780136	13.912891	13.102416	8.855044	12.132081	13.883285	13.505673	12.793871	12.412276	13.631579	13.305122	13.9384	11.657154	13.384231	13.489636	14.395377	14.241587	14.075526	10.747824	13.652056	13.197784	14.447755	14.530513	14.697435	13.583884	14.430207	13.157226	14.372309	14.098441	14.312806	13.964909	14.397525	14.340858	14.039948	13.879531	14.219962	12.821339	14.088046	13.361979	13.815191	14.230721	14.354662	14.278793	14.606532	13.873278	14.243377	13.873586	13.428752	14.159625	13.744516	13.26102	13.382873	14.137183	14.520612	7.802869	12.931742	12.21636	12.353493	12.982377	13.621288	12.461671	13.97395	12.606646	13.683546	12.925924	13.321749	11.0191765	13.517595	13.394469	13.009084	13.810885	12.885342	13.739392	12.864643	14.780031	13.693244
Actb	13.734045	12.522702	13.415247	13.209179	13.056113	13.427856	12.233424	12.887258	12.100704	13.531891	12.820748	13.344985	12.881791	13.600417	13.193485	12.909121	12.898377	12.024754	11.916873	12.750915	12.712462	12.216144	12.8650055	13.198881	12.436209	12.979017	13.691905	12.981638	13.596682	13.925964	11.406142	11.392529	11.683775	12.498023	13.2407055	13.492172	12.483022	12.86085	13.798057	13.232794	13.4168215	13.67703	12.831323	12.208617	12.471388	12.495183	10.690617	13.370143	11.88587	12.386993	12.851105	12.733126	12.636833	12.771518	13.847279	11.798535	12.360644	12.70858	13.827396	13.02202	13.804238	13.682791	13.697816	12.573817	13.623024	13.996163	13.205558	12.876404	13.358192	13.741376	12.387267	13.781056	13.447055	12.4737835	13.336255	13.072685	13.427544	13.44005	13.24184	12.664533	13.154788	13.851693	13.790402	13.273282	13.298301	12.900905	12.929553	13.634423	12.551163	13.090133	12.3847685	12.960629	13.741804	12.916892	13.483205	13.740919	13.832967	13.17246	13.151069	12.881266	10.665685	13.410287	13.370143	13.531974	12.681854	13.1707115	13.693974	13.509228	13.373445	12.707652	13.544967	13.464905	13.361031	13.549852	12.602942	12.342437	13.774049	13.086433	12.778293	11.285748	13.34921	13.488236	13.065959	13.689075	12.808765	13.168075	12.566755	13.537743	13.030241	11.23233	13.2493515	12.19135	11.618789	13.065174	12.070052	12.0976305	12.527914	11.982373	12.535155	13.509334	12.60165	13.029723	13.205435	12.264651	11.777064	12.492098	13.622455	13.485834	13.442727	12.908158	13.396787	13.247888	12.80231	11.660072	12.696563	13.476233	12.894774	12.546756	13.1079035	11.710168	11.094819	11.648384	12.378503	14.788173	12.228651	12.184576	12.879168	11.873432	11.096444	12.660482	11.962317	12.708602	13.085022	12.988616	12.309216	12.827122	11.575345	12.236113	15.5218	13.681532	12.984964	16.016819	14.627011	15.249339	15.783489

...
</pre>
