var ty=function(t){return function(r){for(var e=r.length,n=new Array(e),a=0;a<e;a++)n[a]=t(r[a]);return n}};var ti={compose:function(t){return function(r){return function(e){return t(r(e))}}}},Co=function(t){return t.compose};var j=function(t){return t.identity},K={identity:function(t){return t},Semigroupoid0:function(){return ti}};var Qr=!0;var Tt=function(t){return function(r){return function(e){return t(e)(r)}}},E=function(t){return function(r){return t}};var Bi=function(t){return function(r){return r(t)}},nf=function(t){return function(r){return t(r)}};var D=function(){function t(){}return t.value=new t,t}();var p=function(t){return t.map},Wr=function(t){return function(r){return function(e){return p(t)(e)(r)}}},tr=function(t){return p(t)(E(void 0))},V=function(t){return function(r){return function(e){return p(t)(E(e))(r)}}},fp=function(t){return function(r){return p(t)(E(r))}};var ja={map:Co(ti)},hr={map:ty},xv=function(t){return function(r){return function(e){return p(t)(function(n){return n(e)})(r)}}};var ry=function(t){return function(r){return t.length===0?r:r.length===0?t:t.concat(r)}};var ge=function(t){return t.reflectSymbol};var Ac=function(t){var r=function(e){var n;function a(u){e=u}for(;;)n=a(e);return n};return r(t)};var _p=function(t){return function(r){return{}.hasOwnProperty.call(r,t)}},Xa=function(t){return function(r){return r[t]}},ju=function(t){return function(r){return function(e){var n={};for(var a in e)({}).hasOwnProperty.call(e,a)&&(n[a]=e[a]);return n[t]=r,n}}};var ey={append:function(t){return function(r){return void 0}}};var Pe={append:ry};var _t=function(t){return t.append},Fv=function(t){return{append:function(r){return function(e){return function(n){return _t(t)(r(n))(e(n))}}}}};var O=function(t){return t.alt};var ny=function(t){return function(r){for(var e=t.length,n=r.length,a=new Array(e*n),u=0,o=0;o<e;o++)for(var i=t[o],_=0;_<n;_++)a[u++]=i(r[_]);return a}};var kl={apply:ny,Functor0:function(){return hr}},$t=function(t){return t.apply};var X=function(t){return function(r){return function(e){return $t(t)(p(t.Functor0())(E(j(K)))(r))(e)}}},Gn=function(t){return function(r){return function(e){return function(n){return $t(t)(p(t.Functor0())(r)(e))(n)}}}};var f=function(t){return t.pure};var On=function(t){return function(r){return function(e){if(r)return e;if(!r)return f(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[r.constructor.name,e.constructor.name])}}},gl=function(t){return function(r){return function(e){return $t(t.Apply0())(f(t)(r))(e)}}};var ce={pure:function(t){return[t]},Apply0:function(){return kl}};var ay=function(t){return function(r){for(var e=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(e,r(t[n]));return e}};var ri={bind:ay,Apply0:function(){return kl}},ct=function(t){return t.bind},Jn=function(t){return Tt(ct(t))};var af=function(t){return function(r){return function(e){return function(n){return ct(t)(r(n))(e)}}}};var ei=function(t){return function(r){return ct(t)(r)(j(K))}};var un=function(t){return function(r){for(var e=t>r?-1:1,n=new Array(e*(r-t)+1),a=t,u=0;a!==r;)n[u++]=a,a+=e;return n[u]=a,n}},GT=function(t){return function(r){if(t<1)return[];var e=new Array(t);return e.fill(r)}},JT=function(t){return function(r){for(var e=[],n=0,a=0;a<t;a++)e[n++]=r;return e}},pp=typeof Array.prototype.fill=="function"?GT:JT,jT=function(){function t(a,u){this.head=a,this.tail=u}var r={};function e(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],o=0,i=a;i!==r;)u[o++]=i.head,i=i.tail;return u}return function(a){return function(u){return n(a(e)(r)(u))}}}(),Aa=function(t){return t.length};var uy=function(t){return function(r){return function(e){return function(n){return n<0||n>=e.length?r:t(e[n])}}}};var oy=function(t){return function(r){return function(e){return function(n){for(var a=0,u=n.length;a<u;a++)if(e(n[a]))return t(a);return r}}}};var iy=function(t){return function(r){return function(e){return function(n){if(e<0||e>=n.length)return r;var a=n.slice();return a.splice(e,1),t(a)}}}};var XT=function(){function t(r,e,n,a,u,o){var i,_,m,l,s,v,c;for(i=u+(o-u>>1),i-u>1&&t(r,e,a,n,u,i),o-i>1&&t(r,e,a,n,i,o),_=u,m=i,l=u;_<i&&m<o;)s=a[_],v=a[m],c=e(r(s)(v)),c>0?(n[l++]=v,++m):(n[l++]=s,++_);for(;_<i;)n[l++]=a[_++];for(;m<o;)n[l++]=a[m++]}return function(r){return function(e){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(r,e,a,n.slice(0),0,n.length),a)}}}}();var uf=function(t){return function(r){return function(e){for(var n=r.length<e.length?r.length:e.length,a=new Array(n),u=0;u<n;u++)a[u]=t(r[u])(e[u]);return a}}};var cy=function(t){return function(r){return t[r]}};var KT=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var fy={defer:function(t){return function(r){return t(void 0)(r)}}},of=function(t){return t.defer},wv=function(t){return function(r){var e=KT("go","Control.Lazy",function(){return of(t)(function(a){return r(e(25))})}),n=e(25);return n}};var $u=function(t){return function(r){return function(e){return ct(t.Bind1())(r)(function(n){return ct(t.Bind1())(e)(function(a){return f(t.Applicative0())(n(a))})})}}};var YT=String.fromCharCode(65535),ZT=String.fromCharCode(0),tS=Number.POSITIVE_INFINITY,rS=Number.NEGATIVE_INFINITY;var ly=function(t){return function(r){return function(e){return function(n){return function(a){return n<a?t:n===a?r:e}}}}};var _y=ly,py=ly;var sy=function(t){return function(r){return t===r}};var vy=sy,my=sy;var Cl={eq:my},Ui={eq:vy};var Jt=function(t){return t.eq};var jt=function(){function t(){}return t.value=new t,t}(),_r=function(){function t(){}return t.value=new t,t}(),pr=function(){function t(){}return t.value=new t,t}();var Dy=function(t){return function(r){return t-r|0}},dy=function(t){return function(r){return t-r}};var by=function(t){return function(r){return t+r|0}},yy=function(t){return function(r){return t*r|0}},Ay=function(t){return function(r){return t+r}},ky=function(t){return function(r){return t*r}};var ia=function(t){return t.zero};var ka={add:Ay,zero:0,mul:ky,one:1},Qa={add:by,zero:0,mul:yy,one:1};var ca=function(t){return t.one};var An=function(t){return t.mul};var Sr=function(t){return t.add};var pu=function(t){return t.sub};var kc={sub:dy,Semiring0:function(){return ka}},Mv={sub:Dy,Semiring0:function(){return Qa}};var hl=function(t){return function(r){return pu(t)(ia(t.Semiring0()))(r)}};var Ra=function(){return{compare:py(jt.value)(pr.value)(_r.value),Eq0:function(){return Cl}}}(),ze=function(){return{compare:_y(jt.value)(pr.value)(_r.value),Eq0:function(){return Ui}}}();var Xt=function(t){return t.compare};var Cy=function(t){return function(r){return function(e){var n=Xt(t)(r)(e);return!(n instanceof jt)}}};var wu=function(t){return function(r){return function(e){var n=Xt(t)(r)(e);if(n instanceof jt)return e;if(n instanceof pr||n instanceof _r)return r;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var Iv=function(t){return function(r){return function(e){var n=Cy(t)(e)(ia(r.Semiring0()));return n?e:hl(r)(e)}}};var jn=function(t){return t.top};var Cc={top:2147483647,bottom:-2147483648,Ord0:function(){return ze}};var Xn=function(t){return t.bottom};var Ey=function(t){return t.toString()};var Na={show:Ey};var Wt=function(t){return t.show};var L=function(){function t(){}return t.value=new t,t}(),T=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Nt=function(t){return function(r){return function(e){if(e instanceof L)return t;if(e instanceof T)return r(e.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}};var Ee={map:function(t){return function(r){return r instanceof T?new T(t(r.value0)):L.value}}};var fa=function(t){return Nt(t)(j(K))},Kn=function(){return function(t){if(t instanceof T)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var qi={apply:function(t){return function(r){if(t instanceof T)return p(Ee)(t.value0)(r);if(t instanceof L)return L.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,r.constructor.name])}},Functor0:function(){return Ee}},ga={bind:function(t){return function(r){if(t instanceof T)return r(t.value0);if(t instanceof L)return L.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,r.constructor.name])}},Apply0:function(){return qi}};var Eo=function(){return{pure:T.create,Apply0:function(){return qi}}}();var Qt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Kt=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var Ec={map:function(t){return function(r){if(r instanceof Qt)return new Qt(r.value0);if(r instanceof Kt)return new Kt(t(r.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[r.constructor.name])}}};var La=function(t){return function(r){return function(e){if(e instanceof Qt)return t(e.value0);if(e instanceof Kt)return r(e.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},vp=function(){return La(E(L.value))(T.create)}();var su=function(t){return t};var Qu={map:function(t){return function(r){return t(r)}}};var Ty={apply:function(t){return function(r){return t(r)}},Functor0:function(){return Qu}},AS={bind:function(t){return function(r){return r(t)}},Apply0:function(){return Ty}},Nv={pure:su,Apply0:function(){return Ty}},Mu={Applicative0:function(){return Nv},Bind1:function(){return AS}};var Sy=function(t){return Math.min(Math.abs(t),2147483647)},xy=function(t){return function(r){return r===0?0:r>0?Math.floor(t/r):-Math.floor(t/-r)}},Fy=function(t){return function(r){if(r===0)return 0;var e=Math.abs(r);return(t%e+e)%e}},Oy=function(t){return function(r){return t/r}};var $y={Ring0:function(){return kc}},wy={Ring0:function(){return Mv}};var Ya=function(t){return t.mod};var Fl={degree:function(t){return 1},div:Oy,mod:function(t){return function(r){return 0}},CommutativeRing0:function(){return $y}},Ku={degree:Sy,div:xy,mod:Fy,CommutativeRing0:function(){return wy}},Yu=function(t){return t.div};var Ol={mempty:void 0,Semigroup0:function(){return ey}};var vr=function(t){return t.mempty},mp=function(t){return{mempty:function(r){return vr(t)},Semigroup0:function(){return Fv(t.Semigroup0())}}};var Lv=function(t){return function(){return t}},My=function(t){return function(r){return function(){return r(t())()}}};var Pu=function(t){return function(r){return function(){for(var e=0,n=t.length;e<n;e++)r(t[e])()}}};var Py=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},ni={Applicative0:function(){return ft},Bind1:function(){return Yn}},Yn={bind:My,Apply0:function(){return Bv(0)}},ft={pure:Lv,Apply0:function(){return Bv(0)}},Iy=Py("functorEffect","Effect",function(){return{map:gl(ft)}}),Bv=Py("applyEffect","Effect",function(){return{apply:$u(ni),Functor0:function(){return Iy(0)}}}),R=Iy(20),st=Bv(23),Ry=function(t){return{append:Gn(st)(_t(t))}},Tc=function(t){return{mempty:Lv(vr(t)),Semigroup0:function(){return Ry(t.Semigroup0())}}};var Ny=function(t){return function(){return{value:t}}};var Vr=function(t){return function(){return t.value}},Ly=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},se=function(t){return function(r){return function(){r.value=t}}};var Nr=Ny,TS=Ly,To=function(t){return TS(function(r){var e=t(r);return{state:e,value:e}})},ai=function(t){return function(r){return tr(R)(To(t)(r))}};var By=function(t){return function(r){return function(){return t(r())}}},Uy=function(t){return function(){return t}},Wy=function(t){return function(r){return function(){return r(t())()}}};function Oe(t){return function(){return{value:t}}}var Qe=function(t){return function(){return t.value}},qy=function(t){return function(r){return function(){var e=t(r.value);return r.value=e.state,e.value}}},mu=function(t){return function(r){return function(){return r.value=t}}};var wS=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},MS=qy,tu=function(t){return MS(function(r){var e=t(r);return{state:e,value:e}})},rr={map:By},Wv={Applicative0:function(){return sr},Bind1:function(){return Sc}},Sc={bind:Wy,Apply0:function(){return qv(0)}},sr={pure:Uy,Apply0:function(){return qv(0)}},qv=wS("applyST","Control.Monad.ST.Internal",function(){return{apply:$u(Wv),Functor0:function(){return rr}}}),ae=qv(46);function xc(){return[]}var Hv=function(t){return function(r){return function(){return r.push.apply(r,t)}}};var bp=function(t){return function(r){return function(e){return function(n){return function(){return n.splice.apply(n,[t,r].concat(e))}}}}};function PS(t){return function(){return t.slice()}}var yp=PS;var IS=function(){function t(r,e,n,a,u,o){var i,_,m,l,s,v,c;for(i=u+(o-u>>1),i-u>1&&t(r,e,a,n,u,i),o-i>1&&t(r,e,a,n,i,o),_=u,m=i,l=u;_<i&&m<o;)s=a[_],v=a[m],c=e(r(s)(v)),c>0?(n[l++]=v,++m):(n[l++]=s,++_);for(;_<i;)n[l++]=a[_++];for(;m<o;)n[l++]=a[m++]}return function(r){return function(e){return function(n){return function(){return n.length<2||t(r,e,n,n.slice(0),0,n.length),n}}}}}();var Hi=function(t){return Hv([t])};var Jy=function(t){return function(r){return t&&r}},jy=function(t){return function(r){return t||r}},Xy=function(t){return!t};var ru=function(t){return t.not};var zi=function(t){return t.disj},Ba={ff:!1,tt:!0,implies:function(t){return function(r){return zi(Ba)(ru(Ba)(t))(r)}},conj:Jy,disj:jy,not:Xy};var Ky=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=a-1;u>=0;u--)n=t(e[u])(n);return n}}},Yy=function(t){return function(r){return function(e){for(var n=r,a=e.length,u=0;u<a;u++)n=t(n)(e[u]);return n}}};var x=function(t){return t.empty};var Q=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),pf=function(t){return function(r){return t(r.value0)(r.value1)}};var Ke=function(t){return t.value1};var So={map:function(t){return function(r){return new Q(r.value0,t(r.value1))}}};var Ua=function(t){return t.value0};var Fc=function(t){return function(r){return function(e){return t(new Q(r,e))}}};var ot=function(t){return t};var wn=function(){return ot};var Ye=wn,De=wn;var Xv=function(){return function(){return function(t){return wn()}}};var Kr=function(t){return t.foldr};var be=function(t){return function(r){return Kr(t)(O(r.Alt0()))(x(r))}},on=function(t){return function(r){return function(e){return Kr(t)(function(){var n=O(r.Alt0());return function(a){return n(e(a))}}())(x(r))}}},$e=function(t){return function(r){return function(e){return Kr(r)(function(){var n=X(t.Apply0());return function(a){return n(e(a))}}())(f(t)(void 0))}}},kn=function(t){return function(r){return Tt($e(t)(r))}};var ve=function(t){return t.foldl};var jr={foldr:function(t){return function(r){return function(e){if(e instanceof L)return r;if(e instanceof T)return t(e.value0)(r);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof L)return r;if(e instanceof T)return t(r)(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,r.constructor.name,e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof L)return vr(t);if(e instanceof T)return r(e.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[r.constructor.name,e.constructor.name])}}}};var nA=function(t){return function(r){return function(e){return Kr(t)(function(n){return function(a){return _t(r.Semigroup0())(e(n))(a)}})(vr(r))}}},Lt={foldr:Ky,foldl:Yy,foldMap:function(t){return nA(Lt)(t)}};var gn=function(t){return t.foldMap};var aA=function(){function t(a){return[a]}function r(a){return function(u){return[a,u]}}function e(a){return function(u){return function(o){return[a,u,o]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(o){return function(i){return function(_){function m(l,s){switch(s-l){case 0:return o([]);case 1:return u(t)(i(_[l]));case 2:return a(u(r)(i(_[l])))(i(_[l+1]));case 3:return a(a(u(e)(i(_[l])))(i(_[l+1])))(i(_[l+2]));default:var v=l+Math.floor((s-l)/4)*2;return a(u(n)(m(l,v)))(m(v,s))}}return m(0,_.length)}}}}}}();var Mn=function(t){return t.traverse};var Ix=function(t){return function(r){return Mn(t)(r)(j(K))}},ro={traverse:function(t){return aA($t(t.Apply0()))(p(t.Apply0().Functor0()))(f(t))},sequence:function(t){return Ix(ro)(t)},Functor0:function(){return hr},Foldable1:function(){return Lt}};var Wl=function(){return uf(Q.create)}();var fm=function(){return cy};var bA=function(t){return[t]};var ql=function(){return uy(T.create)(L.value)}(),lm=function(t){return ql(t)(Aa(t)-1|0)};var yA=function(){return oy(T.create)(L.value)}();var _m=function(){return iy(T.create)(L.value)}(),bf=function(t){return function(r){return function(e){return e.length===0?[]:Nt(e)(function(n){return Kn()(_m(n)(e))})(yA(t(r))(e))}}};var Ji=function(t){return function(r){return _t(Pe)([t])(r)}};var AA=function(t){return function(r){for(var e=r.length,n=Array(e),a=0;a<e;a++)n[a]=t(a)(r[a]);return n}};var Fo=function(t){return t.mapWithIndex};var ii={mapWithIndex:AA,Functor0:function(){return hr}};var Oo=function(t){return t.foldrWithIndex};var Iu=function(t){return t.foldlWithIndex};var ci=function(t){return t.foldMapWithIndex};var ji=function(t){return t.traverseWithIndex};var ao=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var Op=function(t){return function(r){return new ao(r,x(t))}};var Te=function(){function t(){}return t.value=new t,t}(),or=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}(),$p=function(t){return t},_F=function(t){return new or(t.value0,t.value1)};var pF=function(t){var r=function(e){return function(n){var a=e,u=!1,o;function i(_,m){if(m instanceof or&&m.value1 instanceof or&&m.value1.value1 instanceof or){a=new or(m,_),n=m.value1.value1.value1;return}var l=function(v){return v instanceof or&&v.value1 instanceof or&&v.value1.value1 instanceof Te?new or(t(v.value0),new or(t(v.value1.value0),Te.value)):v instanceof or&&v.value1 instanceof Te?new or(t(v.value0),Te.value):Te.value},s=function(v){return function(c){var d=v,rt=!1,Z;function Vt(zt,Jr){if(zt instanceof or&&zt.value0 instanceof or&&zt.value0.value1 instanceof or&&zt.value0.value1.value1 instanceof or){d=zt.value1,c=new or(t(zt.value0.value0),new or(t(zt.value0.value1.value0),new or(t(zt.value0.value1.value1.value0),Jr)));return}return rt=!0,Jr}for(;!rt;)Z=Vt(d,c);return Z}};return u=!0,s(_)(l(m))}for(;!u;)o=i(a,n);return o}};return r(Te.value)},wp={map:pF};var Wa={foldr:function(t){return function(r){var e=function(){var a=function(u){return function(o){var i=u,_=!1,m;function l(s,v){if(v instanceof Te)return _=!0,s;if(v instanceof or){i=new or(v.value0,s),o=v.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[s.constructor.name,v.constructor.name])}for(;!_;)m=l(i,o);return m}};return a(Te.value)}(),n=ve(Wa)(Tt(t))(r);return function(a){return n(e(a))}}},foldl:function(t){var r=function(e){return function(n){var a=e,u=!1,o;function i(_,m){if(m instanceof Te)return u=!0,_;if(m instanceof or){a=t(_)(m.value0),n=m.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[m.constructor.name])}for(;!u;)o=i(a,n);return o}};return r},foldMap:function(t){return function(r){return ve(Wa)(function(e){var n=_t(t.Semigroup0())(e);return function(a){return n(r(a))}})(vr(t))}}};var Hl={append:function(t){return function(r){return Kr(Wa)(or.create)(r)(t)}}};var vm={append:function(t){return function(r){return new ao(t.value0,_t(Hl)(t.value1)(_F(r)))}}};var gA={alt:_t(Hl),Functor0:function(){return wp}},mm=function(){return{empty:Te.value,Alt0:function(){return gA}}}();var xA=function(t){return t()};var FA=function(t){throw new Error(t)};var OA=function(){return FA};var MF=xA,du=function(t){return MF(function(){return OA()(t)})};var qt=function(){function t(){}return t.value=new t,t}(),cr=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}(),Pr=function(){function t(r,e,n,a,u,o,i){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o,this.value6=i}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return new t(r,e,n,a,u,o,i)}}}}}}},t}(),Xi=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),_i=function(){function t(r,e,n){this.value0=r,this.value1=e,this.value2=n}return t.create=function(r){return function(e){return function(n){return new t(r,e,n)}}},t}(),Qi=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),wo=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),Ki=function(){function t(r,e,n,a,u,o){this.value0=r,this.value1=e,this.value2=n,this.value3=a,this.value4=u,this.value5=o}return t.create=function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return new t(r,e,n,a,u,o)}}}}}},t}(),Pp=function(){function t(r,e,n,a){this.value0=r,this.value1=e,this.value2=n,this.value3=a}return t.create=function(r){return function(e){return function(n){return function(a){return new t(r,e,n,a)}}}},t}();var wA=function(t){return function(r){return new cr(qt.value,t,r,qt.value)}};var Rp=function(t){return function(r){var e=Xt(t),n=function(a){var u=!1,o;function i(_){if(_ instanceof qt)return u=!0,L.value;if(_ instanceof cr){var m=e(r)(_.value1);if(m instanceof pr)return u=!0,new T(_.value2);if(m instanceof jt){a=_.value0;return}a=_.value3;return}if(_ instanceof Pr){var l=e(r)(_.value1);if(l instanceof pr)return u=!0,new T(_.value2);var s=e(r)(_.value4);if(s instanceof pr)return u=!0,new T(_.value5);if(l instanceof jt){a=_.value0;return}if(s instanceof _r){a=_.value6;return}a=_.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[_.constructor.name])}for(;!u;)o=i(a);return o};return n}};var MA=function(t){return t instanceof qt};var Ze=function(t){return function(r){return function(e){var n=t,a=r,u=!1,o;function i(_,m,l){if(m instanceof Te)return u=!0,l;if(m instanceof or){if(m.value0 instanceof Xi){n=_,a=m.value1,e=new cr(l,m.value0.value0,m.value0.value1,m.value0.value2);return}if(m.value0 instanceof _i){n=_,a=m.value1,e=new cr(m.value0.value0,m.value0.value1,m.value0.value2,l);return}if(m.value0 instanceof Qi){n=_,a=m.value1,e=new Pr(l,m.value0.value0,m.value0.value1,m.value0.value2,m.value0.value3,m.value0.value4,m.value0.value5);return}if(m.value0 instanceof wo){n=_,a=m.value1,e=new Pr(m.value0.value0,m.value0.value1,m.value0.value2,l,m.value0.value3,m.value0.value4,m.value0.value5);return}if(m.value0 instanceof Ki){n=_,a=m.value1,e=new Pr(m.value0.value0,m.value0.value1,m.value0.value2,m.value0.value3,m.value0.value4,m.value0.value5,l);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[m.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[m.constructor.name,l.constructor.name])}for(;!u;)o=i(n,a,e);return o}}},Vl=function(t){return function(r){return function(e){var n=function(o){return function(i){var _=o,m=!1,l;function s(v,c){if(v instanceof Te)return m=!0,new cr(c.value0,c.value1,c.value2,c.value3);if(v instanceof or){if(v.value0 instanceof Xi)return m=!0,Ze(t)(v.value1)(new Pr(c.value0,c.value1,c.value2,c.value3,v.value0.value0,v.value0.value1,v.value0.value2));if(v.value0 instanceof _i)return m=!0,Ze(t)(v.value1)(new Pr(v.value0.value0,v.value0.value1,v.value0.value2,c.value0,c.value1,c.value2,c.value3));if(v.value0 instanceof Qi){_=v.value1,i=new Pp(new cr(c.value0,c.value1,c.value2,c.value3),v.value0.value0,v.value0.value1,new cr(v.value0.value2,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof wo){_=v.value1,i=new Pp(new cr(v.value0.value0,v.value0.value1,v.value0.value2,c.value0),c.value1,c.value2,new cr(c.value3,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof Ki){_=v.value1,i=new Pp(new cr(v.value0.value0,v.value0.value1,v.value0.value2,v.value0.value3),v.value0.value4,v.value0.value5,new cr(c.value0,c.value1,c.value2,c.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[v.value0.constructor.name,c.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[v.constructor.name,c.constructor.name])}for(;!m;)l=s(_,i);return l}},a=Xt(t),u=function(o){return function(i){var _=o,m=!1,l;function s(v,c){if(c instanceof qt)return m=!0,n(v)(new Pp(qt.value,r,e,qt.value));if(c instanceof cr){var d=a(r)(c.value1);if(d instanceof pr)return m=!0,Ze(t)(v)(new cr(c.value0,r,e,c.value3));if(d instanceof jt){_=new or(new Xi(c.value1,c.value2,c.value3),v),i=c.value0;return}_=new or(new _i(c.value0,c.value1,c.value2),v),i=c.value3;return}if(c instanceof Pr){var rt=a(r)(c.value1);if(rt instanceof pr)return m=!0,Ze(t)(v)(new Pr(c.value0,r,e,c.value3,c.value4,c.value5,c.value6));var Z=a(r)(c.value4);if(Z instanceof pr)return m=!0,Ze(t)(v)(new Pr(c.value0,c.value1,c.value2,c.value3,r,e,c.value6));if(rt instanceof jt){_=new or(new Qi(c.value1,c.value2,c.value3,c.value4,c.value5,c.value6),v),i=c.value0;return}if(rt instanceof _r&&Z instanceof jt){_=new or(new wo(c.value0,c.value1,c.value2,c.value4,c.value5,c.value6),v),i=c.value3;return}_=new or(new Ki(c.value0,c.value1,c.value2,c.value3,c.value4,c.value5),v),i=c.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[v.constructor.name,c.constructor.name])}for(;!m;)l=s(_,i);return l}};return u(Te.value)}}},BF=function(t){return function(r){var e=function(i){return function(_){var m=i,l=!1,s;function v(c,d){if(c instanceof Te)return l=!0,d;if(c instanceof or){if(c.value0 instanceof Xi&&c.value0.value2 instanceof qt&&d instanceof qt)return l=!0,Ze(t)(c.value1)(new cr(qt.value,c.value0.value0,c.value0.value1,qt.value));if(c.value0 instanceof _i&&c.value0.value0 instanceof qt&&d instanceof qt)return l=!0,Ze(t)(c.value1)(new cr(qt.value,c.value0.value1,c.value0.value2,qt.value));if(c.value0 instanceof Xi&&c.value0.value2 instanceof cr){m=c.value1,_=new Pr(d,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3);return}if(c.value0 instanceof _i&&c.value0.value0 instanceof cr){m=c.value1,_=new Pr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,d);return}return c.value0 instanceof Xi&&c.value0.value2 instanceof Pr?(l=!0,Ze(t)(c.value1)(new cr(new cr(d,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new cr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6)))):c.value0 instanceof _i&&c.value0.value0 instanceof Pr?(l=!0,Ze(t)(c.value1)(new cr(new cr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new cr(c.value0.value0.value6,c.value0.value1,c.value0.value2,d)))):c.value0 instanceof Qi&&c.value0.value2 instanceof qt&&c.value0.value5 instanceof qt&&d instanceof qt?(l=!0,Ze(t)(c.value1)(new Pr(qt.value,c.value0.value0,c.value0.value1,qt.value,c.value0.value3,c.value0.value4,qt.value))):c.value0 instanceof wo&&c.value0.value0 instanceof qt&&c.value0.value5 instanceof qt&&d instanceof qt?(l=!0,Ze(t)(c.value1)(new Pr(qt.value,c.value0.value1,c.value0.value2,qt.value,c.value0.value3,c.value0.value4,qt.value))):c.value0 instanceof Ki&&c.value0.value0 instanceof qt&&c.value0.value3 instanceof qt&&d instanceof qt?(l=!0,Ze(t)(c.value1)(new Pr(qt.value,c.value0.value1,c.value0.value2,qt.value,c.value0.value4,c.value0.value5,qt.value))):c.value0 instanceof Qi&&c.value0.value2 instanceof cr?(l=!0,Ze(t)(c.value1)(new cr(new Pr(d,c.value0.value0,c.value0.value1,c.value0.value2.value0,c.value0.value2.value1,c.value0.value2.value2,c.value0.value2.value3),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value0 instanceof cr?(l=!0,Ze(t)(c.value1)(new cr(new Pr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3,c.value0.value1,c.value0.value2,d),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value5 instanceof cr?(l=!0,Ze(t)(c.value1)(new cr(c.value0.value0,c.value0.value1,c.value0.value2,new Pr(d,c.value0.value3,c.value0.value4,c.value0.value5.value0,c.value0.value5.value1,c.value0.value5.value2,c.value0.value5.value3)))):c.value0 instanceof Ki&&c.value0.value3 instanceof cr?(l=!0,Ze(t)(c.value1)(new cr(c.value0.value0,c.value0.value1,c.value0.value2,new Pr(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3,c.value0.value4,c.value0.value5,d)))):c.value0 instanceof Qi&&c.value0.value2 instanceof Pr?(l=!0,Ze(t)(c.value1)(new Pr(new cr(d,c.value0.value0,c.value0.value1,c.value0.value2.value0),c.value0.value2.value1,c.value0.value2.value2,new cr(c.value0.value2.value3,c.value0.value2.value4,c.value0.value2.value5,c.value0.value2.value6),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value0 instanceof Pr?(l=!0,Ze(t)(c.value1)(new Pr(new cr(c.value0.value0.value0,c.value0.value0.value1,c.value0.value0.value2,c.value0.value0.value3),c.value0.value0.value4,c.value0.value0.value5,new cr(c.value0.value0.value6,c.value0.value1,c.value0.value2,d),c.value0.value3,c.value0.value4,c.value0.value5))):c.value0 instanceof wo&&c.value0.value5 instanceof Pr?(l=!0,Ze(t)(c.value1)(new Pr(c.value0.value0,c.value0.value1,c.value0.value2,new cr(d,c.value0.value3,c.value0.value4,c.value0.value5.value0),c.value0.value5.value1,c.value0.value5.value2,new cr(c.value0.value5.value3,c.value0.value5.value4,c.value0.value5.value5,c.value0.value5.value6)))):c.value0 instanceof Ki&&c.value0.value3 instanceof Pr?(l=!0,Ze(t)(c.value1)(new Pr(c.value0.value0,c.value0.value1,c.value0.value2,new cr(c.value0.value3.value0,c.value0.value3.value1,c.value0.value3.value2,c.value0.value3.value3),c.value0.value3.value4,c.value0.value3.value5,new cr(c.value0.value3.value6,c.value0.value4,c.value0.value5,d)))):(l=!0,du("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[c.constructor.name])}for(;!l;)s=v(m,_);return s}},n=function(i){return function(_){var m=i,l=!1,s;function v(c,d){if(d instanceof cr&&d.value0 instanceof qt&&d.value3 instanceof qt)return l=!0,e(c)(qt.value);if(d instanceof cr){m=new or(new _i(d.value0,d.value1,d.value2),c),_=d.value3;return}if(d instanceof Pr&&d.value0 instanceof qt&&d.value3 instanceof qt&&d.value6 instanceof qt)return l=!0,e(new or(new _i(qt.value,d.value1,d.value2),c))(qt.value);if(d instanceof Pr){m=new or(new Ki(d.value0,d.value1,d.value2,d.value3,d.value4,d.value5),c),_=d.value6;return}return l=!0,du("The impossible happened in partial function `removeMaxNode`.")}for(;!l;)s=v(m,_);return s}},a=function(i){var _=!1,m;function l(s){if(s instanceof cr&&s.value3 instanceof qt)return _=!0,{key:s.value1,value:s.value2};if(s instanceof cr){i=s.value3;return}if(s instanceof Pr&&s.value6 instanceof qt)return _=!0,{key:s.value4,value:s.value5};if(s instanceof Pr){i=s.value6;return}return _=!0,du("The impossible happened in partial function `maxNode`.")}for(;!_;)m=l(i);return m},u=Xt(t),o=function(i){return function(_){var m=i,l=!1,s;function v(c,d){if(d instanceof qt)return l=!0,L.value;if(d instanceof cr){var rt=u(r)(d.value1);if(d.value3 instanceof qt&&rt instanceof pr)return l=!0,new T(new Q(d.value2,e(c)(qt.value)));if(rt instanceof pr){var Z=a(d.value0);return l=!0,new T(new Q(d.value2,n(new or(new Xi(Z.key,Z.value,d.value3),c))(d.value0)))}if(rt instanceof jt){m=new or(new Xi(d.value1,d.value2,d.value3),c),_=d.value0;return}m=new or(new _i(d.value0,d.value1,d.value2),c),_=d.value3;return}if(d instanceof Pr){var Vt=function(){return d.value0 instanceof qt&&d.value3 instanceof qt&&d.value6 instanceof qt}(),rt=u(r)(d.value4),zt=u(r)(d.value1);if(Vt&&zt instanceof pr)return l=!0,new T(new Q(d.value2,Ze(t)(c)(new cr(qt.value,d.value4,d.value5,qt.value))));if(Vt&&rt instanceof pr)return l=!0,new T(new Q(d.value5,Ze(t)(c)(new cr(qt.value,d.value1,d.value2,qt.value))));if(zt instanceof pr){var Z=a(d.value0);return l=!0,new T(new Q(d.value2,n(new or(new Qi(Z.key,Z.value,d.value3,d.value4,d.value5,d.value6),c))(d.value0)))}if(rt instanceof pr){var Z=a(d.value3);return l=!0,new T(new Q(d.value5,n(new or(new wo(d.value0,d.value1,d.value2,Z.key,Z.value,d.value6),c))(d.value3)))}if(zt instanceof jt){m=new or(new Qi(d.value1,d.value2,d.value3,d.value4,d.value5,d.value6),c),_=d.value0;return}if(zt instanceof _r&&rt instanceof jt){m=new or(new wo(d.value0,d.value1,d.value2,d.value4,d.value5,d.value6),c),_=d.value3;return}m=new or(new Ki(d.value0,d.value1,d.value2,d.value3,d.value4,d.value5),c),_=d.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[d.constructor.name])}for(;!l;)s=v(m,_);return s}};return o(Te.value)}},Ea={foldr:function(t){return function(r){return function(e){if(e instanceof qt)return r;if(e instanceof cr)return Kr(Ea)(t)(t(e.value2)(Kr(Ea)(t)(r)(e.value3)))(e.value0);if(e instanceof Pr)return Kr(Ea)(t)(t(e.value2)(Kr(Ea)(t)(t(e.value5)(Kr(Ea)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[e.constructor.name])}}},foldl:function(t){return function(r){return function(e){if(e instanceof qt)return r;if(e instanceof cr)return ve(Ea)(t)(t(ve(Ea)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Pr)return ve(Ea)(t)(t(ve(Ea)(t)(t(ve(Ea)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[e.constructor.name])}}},foldMap:function(t){return function(r){return function(e){if(e instanceof qt)return vr(t);if(e instanceof cr)return _t(t.Semigroup0())(gn(Ea)(t)(r)(e.value0))(_t(t.Semigroup0())(r(e.value2))(gn(Ea)(t)(r)(e.value3)));if(e instanceof Pr)return _t(t.Semigroup0())(gn(Ea)(t)(r)(e.value0))(_t(t.Semigroup0())(r(e.value2))(_t(t.Semigroup0())(gn(Ea)(t)(r)(e.value3))(_t(t.Semigroup0())(r(e.value5))(gn(Ea)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[e.constructor.name])}}}},Zn={foldrWithIndex:function(t){return function(r){return function(e){if(e instanceof qt)return r;if(e instanceof cr)return Oo(Zn)(t)(t(e.value1)(e.value2)(Oo(Zn)(t)(r)(e.value3)))(e.value0);if(e instanceof Pr)return Oo(Zn)(t)(t(e.value1)(e.value2)(Oo(Zn)(t)(t(e.value4)(e.value5)(Oo(Zn)(t)(r)(e.value6)))(e.value3)))(e.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[e.constructor.name])}}},foldlWithIndex:function(t){return function(r){return function(e){if(e instanceof qt)return r;if(e instanceof cr)return Iu(Zn)(t)(t(e.value1)(Iu(Zn)(t)(r)(e.value0))(e.value2))(e.value3);if(e instanceof Pr)return Iu(Zn)(t)(t(e.value4)(Iu(Zn)(t)(t(e.value1)(Iu(Zn)(t)(r)(e.value0))(e.value2))(e.value3))(e.value5))(e.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[e.constructor.name])}}},foldMapWithIndex:function(t){return function(r){return function(e){if(e instanceof qt)return vr(t);if(e instanceof cr)return _t(t.Semigroup0())(ci(Zn)(t)(r)(e.value0))(_t(t.Semigroup0())(r(e.value1)(e.value2))(ci(Zn)(t)(r)(e.value3)));if(e instanceof Pr)return _t(t.Semigroup0())(ci(Zn)(t)(r)(e.value0))(_t(t.Semigroup0())(r(e.value1)(e.value2))(_t(t.Semigroup0())(ci(Zn)(t)(r)(e.value3))(_t(t.Semigroup0())(r(e.value4)(e.value5))(ci(Zn)(t)(r)(e.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[e.constructor.name])}}},Foldable0:function(){return Ea}},PA=function(){return Oo(Zn)(function(t){return function(r){return function(e){return new or(t,e)}}})(Te.value)}();var Yi=function(){return qt.value}();var Am=function(t){return function(r){return function(e){return Nt(e)(Ke)(BF(t)(r)(e))}}};var Zi=function(t){return function(r){return function(e){return function(n){var a=r(Rp(t)(e)(n));if(a instanceof L)return Am(t)(e)(n);if(a instanceof T)return Vl(t)(e)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var UF=function(t){return function(r){return function(e){return function(n){var a=function(u){return function(o){return function(i){return Zi(t)(function(){var _=Nt(i)(r(i));return function(m){return T.create(_(m))}}())(u)(o)}}};return Iu(Zn)(a)(n)(e)}}}};var IA=function(t){return UF(t)(E)};var Gl=function(t){return t.partitionMap};var pi=function(t){return t.filterMap};var Jl=function(t){return t.filter};var VF=function(t){return t},jl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Xl=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),GF=function(t){return t},Bp=wn(),b=GF;var B=function(){return jl.create}();var at=function(){return Xl.create}(),Gr=function(){var t=p(ja)(p(R)(E(!0)));return function(r){return VF(t(r))}}(),J=function(t){return t.attr};function BA(t){return()=>t.slice()}function UA(t){return r=>e=>()=>{e[t]=r}}function WA(t){return()=>t.slice()}var qA=ot;var Cn={liftST:qA,Monad0:function(){return ni}},hn=function(t){return t.liftST};var XF=function(t){return function(r){return function(e){return IA(t)(r)(e)}}};var km=function(t){return PA(t)};var VA=function(t){return wA(t)(void 0)};var gm=function(t){return{append:XF(t)}};var GA=function(t){return MA(t)},JA=function(t){return function(r){return function(e){return Vl(t)(r)(void 0)(e)}}};var jA={foldMap:function(t){return function(r){var e=gn(Wa)(t)(r);return function(n){return e(km(n))}}},foldl:function(t){return function(r){var e=ve(Wa)(t)(r);return function(n){return e(km(n))}}},foldr:function(t){return function(r){var e=Kr(Wa)(t)(r);return function(n){return e(km(n))}}}};var Cm=Yi;var XA=function(t){return{mempty:Cm,Semigroup0:function(){return gm(t)}}};var Up=function(t){return function(r){return function(e){return Am(t)(r)(e)}}};function QA(t){return function(r){return function(){return setTimeout(r,t)}}}function KA(t){return function(){clearTimeout(t)}}var Wp=QA;var KF={eq:function(t){return function(r){return t===r}}},qp={compare:function(t){return function(r){return Xt(ze)(t)(r)}},Eq0:function(){return KF}};var Yl=KA;var kf=function(r){return function(e){return r(e)()}};var si=function(t){return t.sampleOn};var me=function(t){return t.keepLatest};var Ru=function(t){return t.fold};var Zl=function(t){return function(r){return function(e){return function(n){return pi(t.Filterable1())(Ke)(Ru(t)(function(a){return function(u){return p(So)(f(Eo))(r(a)(u.value0))}})(e)(new Q(n,L.value)))}}}},zp=function(t){return function(r){var e=function(n){return function(a){if(a instanceof L)return new T({now:n,last:L.value});if(a instanceof T)return new T({now:n,last:new T(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 54, column 3 - line 54, column 50): "+[n.constructor.name,a.constructor.name])}};return pi(t.Filterable1())(j(K))(Ru(t)(e)(r)(L.value))}},t_=function(t){return t.fix};var En=function(t){return function(r){return function(e){return $t(t.Alternative0().Applicative0().Apply0())(p(t.Filterable1().Functor1())(Bi)(r))(e)}}};function Tm(t){return function(r){return t===r}}var gf=Tm;var ek=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var dO=function(t){return function(r){return function(e,n){var a=Nr(L.value)(),u=t(e,function(i){return se(new T(i))(a)()}),o=r(e,function(i){var _=Vr(a)();return kn(ft)(jr)(_)(function(m){return function(){return n(i(m))}})()});return function(){return u(),o()}}}},bO=function(t){return function(r,e){var n=Nr(f(ft)(void 0))(),a=t(r,function(u){var o=Vr(n)();o();var i=u(r,e);return se(i)(n)()});return function(){var o=Vr(n)();return o(),a()}}},k={map:function(t){return function(r){return function(e,n){return r(e,function(a){return n(t(a))})}}}},yO=function(t){return function(r){return function(e){return function(n,a){var u=Nr(e)();return r(n,function(o){var i=To(t(o))(u)();return a(i)})}}}},r_=function(t){return function(r){return function(e,n){return r(e,function(a){var u=t(a);if(u instanceof T)return n(u.value0);if(u instanceof L)return void 0;throw new Error("Failed pattern match at FRP.Event (line 203, column 31 - line 205, column 35): "+[u.constructor.name])})}}},Sm=function(t){return r_(function(r){var e=t(r);if(e)return new T(r);if(!e)return L.value;throw new Error("Failed pattern match at FRP.Event (line 118, column 13 - line 120, column 25): "+[e.constructor.name])})},nk=function(){var r=Nr([])();return{event:function(e,n){return ai(function(a){return _t(Pe)(a)([n])})(r)(),function(){return To(bf(gf)(n))(r)(),void 0}},push:function(e){var n=Vr(r)();return Pu(n)(function(a){return function(){return a(e)}})()}}},AO=function(t){return function(r,e){var n=nk(),a=t(n.event),u=a.input(r,n.push),o=a.output(r,e);return function(){return u(),o()}}},ak={compact:r_(j(K)),separate:function(t){return{left:r_(function(r){if(r instanceof Qt)return new T(r.value0);if(r instanceof Kt)return L.value;throw new Error("Failed pattern match at FRP.Event (line 101, column 13 - line 103, column 33): "+[r.constructor.name])})(t),right:r_(function(r){if(r instanceof Kt)return new T(r.value0);if(r instanceof Qt)return L.value;throw new Error("Failed pattern match at FRP.Event (line 108, column 13 - line 110, column 32): "+[r.constructor.name])})(t)}}},bu={filter:Sm,filterMap:r_,partition:function(t){return function(r){return{yes:Sm(t)(r),no:Sm(function(){var e=ru(Ba);return function(n){return e(t(n))}}())(r)}}},partitionMap:function(t){return function(r){return{left:pi(bu)(function(){var e=La(T.create)(E(L.value));return function(n){return e(t(n))}}())(r),right:pi(bu)(function(e){return vp(t(e))})(r)}}},Compactable0:function(){return ak},Functor1:function(){return k}},kO=function(t){return function(r){return function(e,n){var a=Nr(L.value)(),u=hn(Cn)(xc)(),o=Nr(L.value)(),i=hn(Cn)(xc)(),_=Nr(!0)(),m=t(e,function(c){var d=Vr(_)();if(d)return tr(R)(hn(Cn)(Hi(c)(u)))();se(new T(c))(a)();var rt=Vr(o)();return kn(ft)(jr)(rt)(function(Z){return function(){return n(Z(c))}})()}),l=r(e,function(c){var d=Vr(_)();if(d)return tr(R)(hn(Cn)(Hi(c)(i)))();se(new T(c))(o)();var rt=Vr(a)();return kn(ft)(jr)(rt)(function(Z){return function(){return n(c(Z))}})()});se(!1)(_)();var s=hn(Cn)(yp(u))(),v=hn(Cn)(yp(i))();return function(){return s.length===0?se(lm(v))(o)():Pu(s)(function(c){return function(){return se(new T(c))(a)(),Pu(v)(function(rt){return function(){return se(new T(rt))(o)(),n(rt(c))}})()}})()}(),hn(Cn)(bp(0)(Aa(s))([])(u))(),hn(Cn)(bp(0)(Aa(v))([])(i))(),function(){return m(),l()}}}},Re=function(t){return function(r){return r}(Fm(304).subscribe)(t)},Fm=ek("backdoor","FRP.Event",function(){return{makeEvent:function(){var t=function(r){return function(e,n){return e?f(ft)(void 0):r(function(a){return function(){return n(a)}})()}};return t}(),makePureEvent:function(){var t=function(r){return function(e,n){return r(function(a){return function(){return n(a)}})()}};return t}(),makeLemmingEvent:function(){var t=function(r){return function(e,n){var a=function(u){return function(o){return function(){return u(e,kf(o))}}};return r(a)(function(u){return function(){return n(u)}})()}};return t}(),create:function(){var t=function(){var e=Nr([])();return{event:function(n,a){return ai(function(u){return _t(Pe)(u)([a])})(e)(),function(){return To(bf(gf)(a))(e)(),void 0}},push:function(n){return function(){var u=Vr(e)();return Pu(u)(function(o){return function(){return o(n)}})()}}}};return t}(),createPure:function(){var t=function(){var e=Oe([])();return{event:function(n,a){return hn(Cn)(function(){return tr(rr)(tu(function(o){return _t(Pe)(o)([a])})(e))(),hn(Cn)(function(){return tu(bf(gf)(a))(e)(),void 0})})()},push:function(n){return function(){var u=Qe(e)();return Pu(u)(function(o){return function(){return o(n)}})()}}}};return t}(),subscribe:function(){var t=function(r){return function(e){return function(){return r(!1,kf(e))}}};return t}(),subscribePure:function(){var t=function(){var r=function(e){return function(n){return function(){return e(!0,kf(n))}}};return r}();return t}(),bus:function(){var t=function(r){return function(e,n){var a=xm(624)();return n(r(a.push)(a.event)),f(ft)(void 0)}};return t}(),memoize:function(){var t=function(r){return function(e){return function(n,a){var u=nk();return a(e(u.event)),r(n,u.push)}}};return t}(),hot:function(){var t=function(r){return function(){var n=xm(642)(),a=Re(r)(n.push)();return{event:n.event,unsubscribe:a}}};return t}(),mailboxed:function(){var t=function(r){return function(e){return function(n){return function(a,u){var o=Nr(Yi)();u(n(function(_){return function(m,l){return tr(R)(To(Zi(r)(function(s){if(s instanceof L)return new T([l]);if(s instanceof T)return new T(_t(Pe)(s.value0)([l]));throw new Error("Failed pattern match at FRP.Event (line 655, column 21 - line 657, column 55): "+[s.constructor.name])})(_))(o))(),tr(R)(To(Zi(r)(function(s){if(s instanceof L)return L.value;if(s instanceof T)return new T(bf(gf)(l)(s.value0));throw new Error("Failed pattern match at FRP.Event (line 664, column 21 - line 666, column 69): "+[s.constructor.name])})(_))(o))}}));var i=e(a,function(_){var m=Vr(o)(),l=Rp(r)(_.address)(m);if(l instanceof L)return void 0;if(l instanceof T)return Pu(l.value0)(function(s){return function(){return s(_.payload)}})();throw new Error("Failed pattern match at FRP.Event (line 673, column 13 - line 675, column 70): "+[l.constructor.name])});return function(){return tr(R)(se(Yi)(o))(),i()}}}}};return t}(),delay:function(){var t=function(r){return function(e){return function(n,a){var u=Nr(vr(XA(qp)))(),o=e(n,function(i){var _=Nr(L.value)(),m=Wp(r)(function(){a(i);var s=Vr(_)();return Nt(f(ft)(void 0))(function(v){return ai(Up(qp)(v))(u)})(s)()})();return se(new T(m))(_)(),ai(_t(gm(qp))(VA(m)))(u)()});return function(){var _=Vr(u)();return kn(ft)(jA)(_)(Yl)(),o()}}}};return t}()}}),xm=ek("create","FRP.Event",function(){return function(){return void 0,function(r){return r}(Fm(381).create)()}}),e_=Fm(509),uk=xm(378),Om=function(t){return function(r){return r}(e_.bus)(t)};var Nu=function(t){return function(r){return r}(e_.delay)(t)};var Sa=function(t){return function(r){return r}(e_.makeEvent)(t)},Er=function(t){return function(r){return r}(e_.makeLemmingEvent)(t)};var Mo=function(t){return function(r){return r}(e_.memoize)(t)};var gO={apply:function(t){return function(r){return kO(t)(p(k)(Bi)(r))}},Functor0:function(){return k}};var g={pure:function(t){return function(r,e){return e(t),f(ft)(void 0)}},Apply0:function(){return gO}};var $={alt:function(t){return function(r){return function(e,n){return $t(st)(p(R)(function(a){return function(u){return function(){return a(),u()}}})(function(){return t(e,n)}))(function(){return r(e,n)})()}}},Functor0:function(){return k}};var h={empty:function(t,r){return f(ft)(void 0)},Alt0:function(){return $}},CO={Applicative0:function(){return g},Plus1:function(){return h}},Pt={fold:yO,keepLatest:bO,sampleOn:dO,fix:AO,Alternative0:function(){return CO},Filterable1:function(){return bu}};function n_(t,r){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(e[a]=t[a]);return e}var ik=function(t){return function(){return function(){return function(r){return function(e){return function(n){return ju(ge(t)(r))(e)(n)}}}}}};var ck=function(){return function(){return function(t){return function(r){return n_(t,r)}}}},Rc=function(t){return function(){return function(){return function(r){return function(e){return function(n){return ju(ge(t)(r))(e)(n)}}}}}},Po=function(t){return function(){return function(r){return function(e){return Xa(ge(t)(r))(e)}}}};var Pn={vb:function(t){return f(sr)(new Q({},{}))}},Vp=function(t){return t.vb},xO={vbus:function(){var t=function(){return function(e){return function(n){return function(a){return Er(function(u){return function(o){return function(){var _=Vp(e)(D.value)();return o(a(_.value0)(_.value1))(),f(sr)(void 0)}}})}}}},r=function(){return function(e){return t()(e)}};return r}()},wm=function(){return function(t){return function(r){return function(e){return e()(t)}(xO.vbus)(r)}}},Lu=function(t){return function(){return function(){return function(){return function(r){return function(e){return function(){return function(){return{vb:function(n){return function(){var u=Vp(e)(D.value)(),o=Vp(r)(D.value)();return new Q(Rc(t)()()(D.value)(o.value0)(u.value0),Rc(t)()()(D.value)(o.value1)(u.value1))}}}}}}}}}}},Yr=function(t){return function(){return function(){return function(r){return function(){return function(){return{vb:function(e){return function(){var a=Vp(r)(D.value)(),u=uk();return new Q(Rc(t)()()(D.value)(u.push)(a.value0),Rc(t)()()(D.value)(u.event)(a.value1))}}}}}}}}};var Io=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),a_=function(){function t(){}return t.value=new t,t}();var Cf=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),hf=function(){function t(){}return t.value=new t,t}(),Mm=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Gp=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),Ef=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),mi=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),F=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var fk=function(t){return t};var u_={eq:function(t){return function(r){return t instanceof Io&&r instanceof Io?t.value0===r.value0:t instanceof a_&&r instanceof a_}}};var N=function(t){return new Ef(t)},Jp=function(t){return new mi(t)},jp=function(t){return new Gp(t)};var lk=t=>r=>r[t];var No=function(t){return t.reflectType};var pk=function(t){return No(t)};var o_=hr;var Nc=function(){return function(t){return t}};var Dk=function(t){return[t]};var dk=function(){return function(){return function(){return function(){return function(){return function(t){var r=pk(t);return function(e){return lk(r(e))}}}}}}};var Pm=[];var Lc=function(){return function(){return function(t){return function(r){return Ji(t)(r)}}}};function bk(t){return function(){var r={};for(var e in t)hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}}var Lo={};function Im(t){return t()}function yk(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(t[n]));return e}function Ak(t,r){var e={};for(var n in t)hasOwnProperty.call(t,n)&&(e[n]=r(n)(t[n]));return e}function kk(t){return function(r){return function(e){return function(n){var a=e;function u(i){return function(_){return r(_)(i)(n[i])}}for(var o in n)hasOwnProperty.call(n,o)&&(a=t(a)(u(o)));return a}}}}function i_(t){return function(r){var e=[];for(var n in r)hasOwnProperty.call(r,n)&&e.push(t(n)(r[n]));return e}}var MO=Object.keys||i_(function(t){return function(){return t}});function Rm(t){return function(r){return function(e){return function(){return e[t]=r,e}}}}var Nm=function(t){return function(r){return function(){return delete r[t],r}}};var Lm=i_(function(t){return function(r){return r}});var UO=bk;var Ck=function(t){return function(r){return Im(function(){var n=UO(r)();return t(n)(),n})}};var hk=function(t){return function(r){return Ak(r,t)}};var Di=function(t){return function(r){return Ck(Rm(t)(r))}},Kp={map:function(t){return function(r){return yk(r,t)}}},WO={mapWithIndex:hk,Functor0:function(){return Kp}},Bm=function(){return ot};var Yp=kk(Bi),Ek=function(t){return function(r){return Yp(function(e){return function(n){return function(a){return _t(t.Semigroup0())(e)(r(n)(a))}}})(vr(t))}},c_={foldl:function(t){return Yp(function(r){return function(e){return t(r)}})},foldr:function(t){return function(r){return function(e){return Kr(Lt)(t)(r)(Lm(e))}}},foldMap:function(t){return function(r){return Ek(t)(E(r))}}},Tk={foldlWithIndex:function(t){return Yp(Tt(t))},foldrWithIndex:function(t){return function(r){return function(e){return Kr(Lt)(pf(t))(r)(i_(Q.create)(e))}}},foldMapWithIndex:function(t){return Ek(t)},Foldable0:function(){return c_}},qO={traverseWithIndex:function(t){return function(r){return function(e){return Yp(function(n){return function(a){return function(u){return $t(t.Apply0())(p(t.Apply0().Functor0())(Tt(Di(a)))(n))(r(a)(u))}}})(f(t)(Lo))(e)}}},FunctorWithIndex0:function(){return WO},FoldableWithIndex1:function(){return Tk},Traversable2:function(){return Tf}},Tf={traverse:function(t){var r=ji(qO)(t);return function(e){return r(E(e))}},sequence:function(t){return Mn(Tf)(t)(j(K))},Functor0:function(){return Kp},Foldable1:function(){return c_}};var Um=function(t){return Ck(Nm(t))};var Sk=function(){function t(){}return t.value=new t,t}(),Wm=function(){function t(){}return t.value=new t,t}(),HO=function(){function t(){}return t.value=new t,t}();var xk=function(t){return function(r){var e=function(n){var a=function(u){return function(o){return new Q(o+1|0,new Q(u,o))}};return Zl(Pt)(a)(n)(0)};return new Gp(me(Pt)(Mo(e(r))(function(n){return p(k)(function(a){return O($)(f(g)(new Cf(t(a.value0))))(p(k)(E(hf.value))(Jl(bu)(function(){var u=Jt(Ui)(a.value1+1|0);return function(o){return u(Ke(o))}}())(n)))})(n)})))}};var Uu=function(t){return function(r){return function(e){var n=function(a){return a(r)(e)};return function(a){if(a instanceof Ef)return on(Lt)(h)(Uu(t)(r)(e))(a.value0);if(a instanceof mi)return me(Pt)(p(k)(Uu(t)(r)(e))(a.value0));if(a instanceof F)return n(t.toElt(a.value0));if(a instanceof Gp)return Er(function(u){return function(o){return function(){var _=Oe(Lo)(),m=u(a.value0)(function(l){return function(){var v=t.ids(e)(),c=Oe(f(sr)(void 0))(),d=t.ids(e)(),rt=Oe(f(sr)(void 0))(),Z=Oe([])(),Vt=Oe(f(sr)(void 0))(),zt=p(rr)(Io.create)(t.ids(e))(),Jr=Oe(Sk.value)(),ie=u(l)(function(It){return function(){var Et=Qe(Jr)();if(It instanceof Mm&&Et instanceof Wm)return ct(Sc)(Qe(Z))($e(sr)(Lt)(function(){var Be=t.doLogic(It.value0)(e);return function(ya){return o(Be(ya))}}()))();if(It instanceof hf&&Et instanceof Wm){tr(rr)(mu(HO.value)(Jr))();var ee=function(){var ya=Qe(Z)();kn(sr)(Lt)(ya)(function(Ju){return kn(sr)(jr)(r.parent)(function(Yo){return o(t.disconnectElement(e)({id:Ju,parent:Yo,scope:zt}))})})();var ua=Qe(c)();ua();var Ue=Qe(rt)();return Ue(),tr(rr)(tu(Um(v))(_))(),tr(rr)(tu(Um(d))(_))()};return tr(rr)(mu(ee)(Vt))(),ee()}if(It instanceof Cf&&Et instanceof Sk){tr(rr)(mu(Wm.value)(Jr))();var Hn=u(Uu(t)(function(){var Be={};for(var ya in r)({}).hasOwnProperty.call(r,ya)&&(Be[ya]=r[ya]);return Be.scope=zt,Be.raiseId=function(ua){return tr(rr)(tu(_t(Pe)([ua]))(Z))},Be}())(e)(It.value0))(o)();return tr(rr)(tu(Di(d)(Hn))(_))(),tr(rr)(mu(Hn)(rt))()}return void 0}})();tr(rr)(mu(ie)(c))(),tr(rr)(tu(Di(v)(ie))(_))();var ut=Qe(Vt)();return ut()}})();return function(){return ct(Sc)(Qe(_))(ve(c_)(X(ae))(f(sr)(void 0)))(),m()}}}});throw new Error("Failed pattern match at Bolson.Control (line 523, column 17 - line 608, column 20): "+[a.constructor.name])}}}},zO=function(){return function(t){return function(r){return function(e){return function(n){return function(a){return function(u){var o=function(i){return function(_){return Er(function(m){return function(l){return function(){var v=BA(p(hr)(E(""))(Nc()(a)))(),c=be(Lt)(h)(Fo(ii)(function(ie){return wv(fy)(function(ut){return function(It){return It instanceof F?function(Cr){return Cr(function(){var Et={};for(var ee in i)({}).hasOwnProperty.call(i,ee)&&(Et[ee]=i[ee]);return Et.parent=L.value,Et.scope=r(i.scope),Et.raiseId=function(Hn){return UA(ie)(Hn)(v)},Et}())(_)}(n.toElt(It.value0)):ut(n.wrapElt(It))}})})(Nc()(a))),d=m(c)(l)(),rt=Oe(f(sr)(void 0))(),Z=p(rr)(ot)(WA(v))(),Vt=p(o_)(function(ie){return function(ut){return new F(n.fromEltO1(function(It){return function(Cr){return Er(function(Et){return function(ee){return function(){return It.raiseId(ie)(),kn(sr)(jr)(It.parent)(function(Be){return ee(n.giveNewParent(Cr)({id:ie,parent:Be,scope:It.scope})(ut))})(),f(sr)(void 0)}}})}}))}})(Z),zt=Uu(e)(i)(_)(u(Vt)(ot)),Jr=m(zt)(l)();return tr(rr)(mu(Jr)(rt))(),function(){d(),On(sr)(!t)(kn(sr)(Lt)(Nc()(Z))(function(It){return l(n.deleteFromCache(_)({id:It}))}))();var ut=Qe(rt)();return ut()}}}})}};return new F(n.fromEltO2(o))}}}}}}};var qm=function(){return function(t){return function(r){return function(e){return function(n){return zO()(!1)(j(K))(t)(r)(e)(n)}}}}};var Fk=function(t){return function(r){return function(e){var n=function(a){return function(u){return Er(function(o){return function(i){return function(){var m=Oe(L.value)(),l=e(new F(r.fromElt(function(s){return function(v){return Er(function(c){return function(d){return function(){return function(){var Vt=Qe(m)();if(Vt instanceof L)return void 0;if(Vt instanceof T)return kn(sr)(jr)(s.parent)(function(zt){return On(sr)(Vt.value0!==zt)(X(ae)(s.raiseId(Vt.value0))(d(r.connectToParent(u)({id:Vt.value0,parent:zt}))))})();throw new Error("Failed pattern match at Bolson.Control (line 633, column 27 - line 640, column 16): "+[Vt.constructor.name])}(),f(sr)(void 0)}}})}})));return o(Uu(t)(function(){var s={};for(var v in a)({}).hasOwnProperty.call(a,v)&&(s[v]=a[v]);return s.parent=a.parent,s.scope=a.scope,s.raiseId=function(c){return function(){return a.raiseId(c)(),tr(rr)(mu(new T(c))(m))()}},s}())(u)(l))(i)()}}})}};return new F(r.fromElt(n))}}};var In={dimap:function(t){return function(r){return function(e){return function(n){return r(e(t(n)))}}}}},di=function(t){return t.dimap},qa=function(t){return function(r){return di(t)(r)(j(K))}};var Wu=function(){return function(t){return function(r){return function(e){return new mi(wm()(t)(r)(e))}}}};var Hm=function(t){return Om(t)};var Ve=function(t){return new mi(Hm(t))},Ok=function(t){return Ve(Fc(t))};var JO=function(t){return t.makeText},jO=function(t){return function(r){return function(e){return p(k)(function(n){return t.setText(function(a){return{id:r,text:a}}(n))})(e)}}},XO=function(t){return function(r){return function(e){return p(k)(function(n){return function(a){if(a.value instanceof jl)return t.setProp({id:r,key:a.key,value:a.value.value0});if(a.value instanceof Xl)return t.setCb({id:r,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 86, column 26 - line 88, column 45): "+[a.value.constructor.name])}(Bp(n))})(e)}}},QO=function(t){return t.makeElement},KO=function(t){return t.attributeParent},tn=function(t){var r=function(e){return function(n){return Er(function(a){return function(u){return function(){var i=n.ids();e.raiseId(i)();var _=a(be(Lt)(h)([f(g)(JO(n)({id:i,parent:e.parent,scope:e.scope})),jO(n)(i)(t)]))(u)();return function(){return u(n.deleteFromCache({id:i}))(),_()}}}})}};return new F(r)},Zr=function(t){return tn(f(g)(t))},$k=function(t){return function(r){return function(e){return t(x(h))([xk(r)(e)])}}};var YO=function(){return{doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(){var t=De();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Jt(u_)})}},toElt:function(t){return t}}};var wk=Uu(YO()),Mk=function(t){return function(r){return function(e){return Er(function(n){return function(a){return function(){var o=e.ids();return n(O($)(f(g)(e.makeRoot({id:o,root:t})))(wk({parent:new T(o),scope:new Io("rootScope"),raiseId:function(i){return f(sr)(void 0)},pos:L.value})(e)(r)))(a)()}}})}}};var z=function(t){return function(r){return function(e){var n=function(a){return function(u){return Er(function(o){return function(i){return function(){var m=u.ids();a.raiseId(m)();var l=o(O($)(be(Lt)(h)(_t(Pe)([f(g)(QO(u)({id:m,parent:a.parent,scope:a.scope,tag:t})),XO(u)(m)(r)])(Nt([])(function(s){return[f(g)(KO(u)({id:m,parent:s,pos:a.pos}))]})(a.parent))))(wk({parent:new T(m),scope:a.scope,raiseId:function(s){return f(sr)(void 0)},pos:L.value})(u)(e)))(i)();return function(){return i(u.deleteFromCache({id:m}))(),l()}}}})}};return n}}};var ue=function(){function t(){}return t.value=new t,t}();var fe={attr:function(t){return function(r){return b({key:"click",value:at(r)})}}};var Bt=function(){function t(){}return t.value=new t,t}();var ts={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var Pk={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var lt={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var Ik={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}},Sf={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var zm={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var Rk={attr:function(t){return function(r){return b({key:"style",value:B(r)})}}};var Vm=function(t){return function(r){return new F(z("a")(t)(N(r)))}};var dr=function(t){return function(r){return new F(z("div")(t)(N(r)))}},Lr=dr(x(h));var Ff=function(t){return function(r){return new F(z("span")(t)(N(r)))}},Gm=Ff(x(h));var t$=function(t){return function(r){return Ok(function(e){return Jp(Mo(t(e.value1))(function(n){return r(new Q(e.value0,n))}))})}},Lk=function(t){return t$(function(r){return O($)(f(g)(t))(r)})};var Bk=function(t){return function(r){return t(r)}};var Uk=(t,r,e,n)=>{t(a=>n.units[a].main.appendChild(n.units[r].main))(e)};var Wk=t=>r=>e=>()=>{e.units[r.id].main.parentNode||t(r.pos)(a=>()=>e.units[r.parent].main.children[a]?(e.units[r.parent].main.insertBefore(e.units[r.id].main,e.units[r.parent].main.children[a]),!0):!1)()||(r.parent.indexOf("@!%")!==-1?e.units[r.parent].main.parentNode.replaceChild(e.units[r.id].main,e.units[r.parent].main):e.units[r.parent].main.appendChild(e.units[r.id].main))},qk=t=>r=>e=>n=>()=>{var a,u=e.id;n.scopes[e.scope]||(n.scopes[e.scope]=[]),n.scopes[e.scope].push(u),t(e.parent)(()=>()=>n.hydrating&&r&&(a=document.body.querySelectorAll("[data-deku-ssr-"+u+"]").item(0))?(n.units[u]={listeners:{},parent:e.parent,scope:e.scope,main:a},!0):!1)()||(n.units[u]={listeners:{},parent:e.parent,scope:e.scope,main:document.createElement(e.tag)})},Hk=t=>r=>e=>n=>a=>()=>{var u=n.id,o;a.scopes[n.scope]||(a.scopes[n.scope]=[]),a.scopes[n.scope].push(u),t(n.parent)(_=>()=>{if(a.hydrating&&r&&(o=document.body.querySelectorAll("[data-deku-ssr-"+_+"]").item(0))){var m=0;if(o.childNodes.length===1)o.prepend(document.createTextNode(""));else for(var m=0;m<o.childNodes.length;m++)if(o.childNodes[m].nodeType===8&&o.childNodes[m].nodeValue===u){m=m-1;break}return a.units[u]={main:o.childNodes[m],parent:n.parent,scope:n.scope},!0}return!1})()||(a.units[u]={main:document.createTextNode(""),parent:n.parent,scope:n.scope},Uk(e,u,n.parent,a))};function Jm(){return{units:{},scopes:{}}}var zk=t=>r=>e=>()=>{var n=r.id,a=r.value;e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),e.units[n].main.tagName==="INPUT"&&r.key==="value"?e.units[n].main.value=a:e.units[n].main.tagName==="INPUT"&&r.key==="checked"?e.units[n].main.checked=a==="true":e.units[n].main.setAttribute(r.key,a)},Vk=t=>r=>e=>()=>{var n=r.id,a=r.value;if(e.hydrating&&t&&!e.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(e.units[n]={listeners:{},parent:r.parent,scope:r.scope,main:dom},e.scopes[r.scope]||(e.scopes[r.scope]=[]),e.scopes[r.scope].push(n)),r.key==="@self@")a(e.units[n].main)();else{e.units[n].listeners[r.key]&&e.units[n].main.removeEventListener(r.key,e.units[n].listeners[r.key]);var u=o=>a(o)();e.units[n].main.addEventListener(r.key,u),e.units[n].listeners[r.key]=u}},Gk=t=>r=>()=>{var e=t.id;r.units[e].main.nodeValue=t.text},Jk=t=>r=>e=>n=>a=>()=>{var u,o,i=n.id,_=n.html,m=n.verb,l=n.cache,s=n.parent,v=n.scope,c=n.pxScope;if(!t(n.parent)(()=>()=>a.hydrating&&r&&(u=document.body.querySelectorAll("[data-deku-ssr-"+i+"]").item(0))?(a.units[i]={listeners:{},scope:v,parent:s,main:u},!0):!1)()){let Z=Object.entries(l);for(var rt=0;rt<Z.length;rt++){let Vt=Z[rt][0];Z[rt][1]===!0?_=_.replace(m+Vt+m,'data-deku-attr-internal="'+Vt+'"'):_=_.replace(m+Vt+m,'<span style="display:contents;" data-deku-elt-internal="'+Vt+'"></span>')}o=document.createElement("div"),o.innerHTML=_.trim(),a.units[i]={listeners:{},scope:v,parent:s,main:o.firstChild}}a.scopes[v]||(a.scopes[v]=[]),a.scopes[v].push(i),o||(o=u),o.querySelectorAll("[data-deku-attr-internal]").forEach(function(Z){var Vt=Z.getAttribute("data-deku-attr-internal");let zt=Vt+"@!%"+c;a.units[zt]={listeners:{},main:Z,scope:v},a.scopes[v].push(zt)}),o.querySelectorAll("[data-deku-elt-internal]").forEach(function(Z){var Vt=Z.getAttribute("data-deku-elt-internal");let zt=Vt+"@!%"+c;a.units[Vt+"@!%"+c]={listeners:{},main:Z,scope:v},a.scopes[v].push(zt)}),u||Uk(e,i,s,a)},jk=t=>r=>()=>{var e=t.id;r.units[e]={main:t.root}},Xk=t=>r=>()=>{var e=t.id,n=t.parent;r.units[e].containingScope=t.scope,r.units[n].main.prepend(r.units[e].main)},Qk=t=>r=>()=>{var e=t.id;r.units[e].noop||r.units[e].containingScope&&!t.scopeEq(r.units[e].containingScope)(t.scope)||r.units[e].main.remove()},Kk=t=>r=>()=>{delete r.units[t.id]},Yk=t=>r=>()=>{var e=t.id,n=t.pos,a=r.units[e].main.parentNode;a.insertBefore(r.units[e].main,a.children.length<=n?a.children[a.children.length-1]:n<0?a.children[0]:a.children[n])};var Zk=function(t){return function(r){return function(e){return(e|0)===e?t(e):r}}},Ur=function(t){return t};var jm=function(t){return function(r){return Math.pow(t,r)|0}};var rs=isFinite;var f_=Math.floor;var rc=function(t){return function(r){return Math.pow(t,r)}},l_=function(t){return function(r){return t%r}},es=Math.round;var ns=Math.sin;var ec=3.141592653589793;var Of=function(){return Zk(T.create)(L.value)}(),rg=function(t){if(!rs(t))return 0;if(t>=Ur(jn(Cc)))return jn(Cc);if(t<=Ur(Xn(Cc)))return Xn(Cc);if(Qr)return fa(0)(Of(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},eg=function(t){return rg(es(t))};var __=function(t){return rg(f_(t))};var Uo=Math.random;var p_=function(t){return function(r){return function(){var n=Uo(),a=(Ur(r)-Ur(t)+1)*n+Ur(t);return __(a)}}};var ng=function(t){return t};var c$=1,as=2147483647,f$=function(){return as-1|0}(),nc=function(t){var r=function(e){return function(n){return function(a){var u=n-e|0,o=Ya(Ku)(a)(u),i=o<e;return i?o+n|0:o}}};return r(c$)(f$)(t)};var l$=0,_$=48271,ag=function(t){return function(r){return Kn()(Of(l_(Ur(_$)*Ur(r)+Ur(t))(Ur(as))))}},ug=ag(l$);var y$=function(){function t(o){this.fn=o}var r={},e=function(o,i){this.head=o,this.tail=i};function n(o){return new e(o,r)}function a(o){return function(i){return new e(o,i)}}function u(o){for(var i=[],_=o;_!==r;)i.push(_.head),_=_.tail;return i}return function(o){return function(i){return function(_){var m=function(s,v){return o(i(a)(_(s)))(v)},l=function(s,v,c){if(v===0)return s;var d=c[v-1];return new t(function(){var rt=l(m(d,s),v-1,c);return rt})};return function(s){for(var v=i(n)(_(s[s.length-1])),c=l(v,s.length-1,s);c instanceof t;)c=c.fn();return i(u)(c)}}}}}();var lg=function(t){return t};var _g=Pe;var pg=Lt;var mg=lg,D_=function(t){return t};var d_=function(t){return mg(bA(t))};var $f=function(t){if(Aa(t)>0)return new T(mg(t));if(Qr)return L.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var Dg=function(t){return function(r){return t(D_(r))}};var dg=Dg(Aa);var bg=function(){return Dg(fm())};var bi=function(t){return t.state};function Wo(t){return new Error(t)}function y_(t){return function(){throw t}}function is(t){return function(r){return function(){try{return r()}catch(e){return e instanceof Error||Object.prototype.toString.call(e)==="[object Error]"?t(e)():t(new Error(e.toString()))()}}}}var oo=function(t){return t.throwError};var tw={throwError:y_,Monad0:function(){return ni}};var aD={catchError:Tt(is),MonadThrow0:function(){return tw}};var yi=function(t){return t.catchError};var A_=function(t){return function(r){return yi(t)(p(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Kt.create)(r))(function(){var e=f(t.MonadThrow0().Monad0().Applicative0());return function(n){return e(Qt.create(n))}}())}};var te={liftEffect:j(K),Monad0:function(){return ni}},le=function(t){return t.liftEffect};var ss=function(t){return{map:function(r){return function(e){return function(n){return p(t)(function(a){return new Q(r(a.value0),a.value1)})(e(n))}}}}};var uD=function(t){return{Applicative0:function(){return Ds(t)},Bind1:function(){return vs(t)}}},vs=function(t){return{bind:function(r){return function(e){return function(n){return ct(t.Bind1())(r(n))(function(a){var u=e(a.value0);return u(a.value1)})}}},Apply0:function(){return ms(t)}}},ms=function(t){return{apply:$u(uD(t)),Functor0:function(){return ss(t.Bind1().Apply0().Functor0())}}},Ds=function(t){return{pure:function(r){return function(e){return f(t.Applicative0())(new Q(r,e))}},Apply0:function(){return ms(t)}}};var oD=function(t){return{state:function(r){var e=f(t.Applicative0());return function(n){return e(r(n))}},Monad0:function(){return uD(t)}}};var Sg=function(t){return function(r){var e=t(r);return e.value0}};var iw=function(t){return t};var Og=function(){var t=function(r){return new Q(ng(r.newSeed),function(){var e={};for(var n in r)({}).hasOwnProperty.call(r,n)&&(e[n]=r[n]);return e.newSeed=ug(r.newSeed),e}())};return bi(oD(Mu))(t)}();var io=ss(Qu),$g=p(io)(function(t){return Ur(t)/Ur(as)})(Og);var ic=function(t){return Sg(iw(t))};var Bc=vs(Mu);var Uc=ms(Mu),Fg=function(t){return function(r){var e=Ur(r),n=Ur(t),a=function(i){return n+l_(i)(e-n+1)},u=p(io)(Ur)(Og),o=$t(Uc)(p(io)(Sr(ka))(u))(p(io)(An(ka)(2))(u));return p(io)(function(i){return __(a(i))})(o)}},cD=function(t){return function(r){var e=t<=r;return e?Fg(t)(r):Fg(r)(t)}};var E_=Ds(Mu);var fD=function(t){return ct(Bc)(cD(0)(dg(t)-1|0))(function(r){return f(E_)(bg()(t)(r))})};var Wc=function(t){return t.arbitrary};var wg={arbitrary:$g};var ds=function(){return{arbitrary:cD(-1e6)(1e6)}}();var ys=function(t){return function(r){return t instanceof T?r(t.value0):f(ft)(!1)}};var Mg=function(t){return{ids:function(){var e=Qe(t)(),n=Wt(Na)(ic(Wc(ds))({newSeed:nc(e),size:5}));return tr(rr)(tu(Sr(Qa)(1))(t))(),n},makeElement:qk(ys)(!1),attributeParent:Wk(ys),makeRoot:jk,makeText:Hk(ys)(!1)(Nt(void 0)),makePursx:Jk(ys)(!1)(Nt(void 0)),setProp:zk(!1),setCb:Vk(!1),setText:Gk,sendToPos:Yk,deleteFromCache:Kk,giveNewParent:Xk,disconnectElement:Qk}};var Ai=function(){return window};function Ig(t,r,e,n){if(typeof window<"u"){var a=window[e];if(a!=null&&n instanceof a)return r(n)}for(var u=n;u!=null;){var o=Object.getPrototypeOf(u),i=o.constructor.name;if(i===e)return r(n);if(i==="Object")return t;u=o}return t}var xt=function(t){return function(r){return Ig(L.value,T.create,t,r)}};function Rg(t,r,e){return t==null?r:e(t)}var Ge=function(t){return Rg(t,L.value,T.create)};var lD=xt("HTMLCanvasElement");function Ug(t){return function(){return t.body}}var Wg=function(){var t=p(R)(Ge);return function(r){return t(Ug(r))}}();var qg=ot;function qc(t){return function(){return t.valueAsNumber}}var Rf=xt("HTMLInputElement");function pD(t){return function(){return t.document}}function gs(t){return function(r){return function(){return r.requestAnimationFrame(t)}}}var sD=ot;var tM=function(t){return function(r){return function(){var n=Jm(),a=Wr(R)(hn(Cn)(Oe(0)))(function(){var u=Mk(t)(r);return function(o){return u(Mg(o))}}())();return Re(a)(function(u){return u(n)})()}}};var rM=function(t){return function(){var e=ct(Yn)(ct(Yn)(Ai)(pD))(Wg)();return Nt(vr(Tc(Tc(Ol))))(function(n){return tM(n)(t)})(p(Ee)(qg)(e))()}},Vg=function(t){return tr(R)(rM(t))};var nM=function(t){return t};var G={pursxToElement:function(t){return function(r){return function(e){return{cache:Lo,element:function(n){return function(a){return x(h)}}}}}}},vD=function(t){return t.pursxToElement},rn=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var o=vD(t)(n)(D.value)(u);return{cache:Di(No(r)(D.value))(!0)(o.cache),element:function(i){return function(_){return O($)(p(k)(qa(In)(Bp)(function(m){if(m.value instanceof jl)return _.setProp({id:No(r)(D.value)+("@!%"+n),key:m.key,value:m.value.value0});if(m.value instanceof Xl)return _.setCb({id:No(r)(D.value)+("@!%"+n),key:m.key,value:m.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4191, column 38 - line 4201, column 24): "+[m.value.constructor.name])}))(Po(e)()(D.value)(u)))(o.element(i)(_))}}}}}}}}}}};var M=nM,ir=function(t){return function(r){return function(){return function(){return function(e){return function(n){return function(a){return function(u){var o=function(i){return function(_){return Er(function(m){return function(l){return function(){var v=_.ids(),c=_.ids();i.raiseId(v)();var d=vD(e)(c)(D.value)(u),rt=m(O($)(f(g)(_.makePursx({id:v,parent:i.parent,cache:d.cache,pxScope:c,scope:i.scope,html:No(t)(a),verb:No(r)(n)})))(d.element(i)(_)))(l)();return function(){return l(_.deleteFromCache({id:v}))(),rt()}}}})}};return new F(o)}}}}}}}},St=function(t){return function(){return function(){return function(r){return ir(t)({reflectType:function(){return"~"}})()()(r)(D.value)}}}};var aM=Uu({doLogic:function(t){return function(r){return function(e){return r.sendToPos({id:e,pos:t})}}},ids:function(){var t=De();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectElement({id:r.id,scope:r.scope,parent:r.parent,scopeEq:Jt(u_)})}},toElt:function(t){return t}}),P=function(){return function(t){return function(r){return function(e){return{pursxToElement:function(n){return function(a){return function(u){var o=Po(e)()(D.value)(u),i=vD(t)(n)(D.value)(u);return{cache:Di(No(r)(D.value))(!1)(i.cache),element:function(_){return function(m){return O($)(aM({parent:new T(No(r)(D.value)+("@!%"+n)),scope:_.scope,raiseId:function(l){return f(sr)(void 0)},pos:_.pos})(m)(o))(i.element(_)(m))}}}}}}}}}}};function Gg(t){var r={};for(var e in t)({}).hasOwnProperty.call(t,e)&&(r[e]=t[e]);return r}function Jg(t){return function(r){return function(e){return e[t]=r,e}}}var mD=ti;var DD=function(){return function(){return function(t){return function(r){return function(e){return function(n){return Jg(ge(t)(r))(e)(n)}}}}}};var dD=K,jg=function(t){return function(r){return t(Gg(r))}},Xg=Tt(jg)({});var mt=function(){return function(){return{defaults:Tt(ck()())}}},oM=function(t){return t.defaults},Dt={convertRecordOptions:function(t){return function(r){return function(e){return j(dD)}}}},Kg=function(t){return t.convertRecordOptions},_a=function(t){return t.convertOptionsWithDefaults},dt=function(){return function(t){return{convertOptions:function(r){return function(e){return Xg(Kg(t)(r)(D.value)(e))}}}}},iM=function(t){return t.convertOptions},bt=function(t){return function(r){return{convertOptionsWithDefaults:function(e){return function(n){var a=oM(r)(n),u=iM(t)(e);return function(o){return a(u(o))}}}}}},cM=function(t){return t.convertOption},q=function(t){return function(r){return function(){return function(){return function(){return function(e){return{convertRecordOptions:function(n){return function(a){return function(u){return Co(mD)(DD()()(e)(D.value)(cM(r)(n)(D.value)(Po(e)()(D.value)(u))))(Kg(t)(n)(D.value)(u))}}}}}}}}}};var lM=function(){return function(){return function(){return function(t){return function(r){return function(e){return _p(e.type)(t)?Xa(e.type)(t)(e.value):r(e)}}}}}};var oe=function(){return function(t){return function(r){return function(e){return{type:ge(t)(r),value:e}}}}};var _M=function(t){return du("Data.Variant: pattern match failure ["+(t.type+"]"))},Ne=function(){return function(){return function(){return function(t){return lM()()()(t)(_M)}}}};var bD=function(){var t=Op(mm);return function(r){return $p(t(r))}}();var uut=typeof Array.from=="function",out=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",iut=typeof String.prototype.fromCodePoint=="function",cut=typeof String.prototype.codePointAt=="function";var ki={proof:function(t){return t},Coercible0:function(){}},AD=function(t){return t.proof};var gu=void 0;var Os=function(t){return t.toInt},aC=function(t){return function(r){return Os(t)(gu)}};var eu={toInt:function(t){return 8}},uC={Nat0:function(){return eu}},Ho={toInt:function(t){return 7}},oC={Nat0:function(){return Ho}},zo={toInt:function(t){return 6}},iC={Nat0:function(){return zo}},Fa={toInt:function(t){return 5}},$s={Nat0:function(){return Fa}},Rn={toInt:function(t){return 4}},ea={Nat0:function(){return Rn}},Nn={toInt:function(t){return 3}},Cu={Nat0:function(){return Nn}},Ln={toInt:function(t){return 2}},hu={Nat0:function(){return Ln}},Bn={toInt:function(t){return 1}},Eu={Nat0:function(){return Bn}},Ce={toInt:function(t){return 0}};var Tr=function(t){return function(){return function(r){return function(){return function(e){return{Nat0:r.Nat1,Pos1:function(){return t}}}}}}};var co={Nat0:function(){return Ho},Nat1:function(){return eu}};var fo={Nat0:function(){return zo},Nat1:function(){return eu}};var lo={Nat0:function(){return Fa},Nat1:function(){return eu}};var _o={Nat0:function(){return Rn},Nat1:function(){return eu}};var pa={Nat0:function(){return Rn},Nat1:function(){return Fa}};var po={Nat0:function(){return Nn},Nat1:function(){return eu}};var sa={Nat0:function(){return Nn},Nat1:function(){return Fa}};var so={Nat0:function(){return Ln},Nat1:function(){return eu}};var va={Nat0:function(){return Ln},Nat1:function(){return Fa}};var vo={Nat0:function(){return Bn},Nat1:function(){return eu}};var ma={Nat0:function(){return Bn},Nat1:function(){return Fa}};var mo={Nat0:function(){return Ce},Nat1:function(){return eu}};var Da={Nat0:function(){return Ce},Nat1:function(){return Fa}};var cC={Nat0:function(){return Ce},Nat1:function(){return eu}};var kD={Nat0:function(){return Ce},Nat1:function(){return Ho}};var gD={Nat0:function(){return Ce},Nat1:function(){return zo}};var S_={Nat0:function(){return Ce},Nat1:function(){return Fa}};var za={Nat0:function(){return Ce},Nat1:function(){return Rn}};var _n={Nat0:function(){return Ce},Nat1:function(){return Nn}};var pn={Nat0:function(){return Ce},Nat1:function(){return Ln}};var sn={Nat0:function(){return Ce},Nat1:function(){return Bn}},Tu={Nat0:function(){return Ce},Nat1:function(){return Ce}};var fC=ro;var ws=function(t){return t};var x_=function(t){return function(){return function(r){return function(e){return r[Os(t)(e)]}}}};var Ms=function(t){return function(r){var e=aC(t)(D.value),n=function(){return e===0?[]:un(0)(e-1|0)}();return p(hr)(r)(n)}};var Hu=[];var xr=function(t){return function(r){return function(e){return Ji(r)(e)}}};var vn={first:function(t){return function(r){return new Q(t(r.value0),r.value1)}},second:p(So),Profunctor0:function(){return In}},Un=function(t){return t.second},Ps=function(t){return t.first};var qM=function(t){return function(r){return function(e){return function(n){return di(e)(t)(r)(n)}}}};var sC=function(){return function(){return function(t){return qM(wn())(wn())(t)}}};var vC=function(){return function(){return function(t){return sC()()(t)}}};var VM=function(t){return function(r){return function(e){return di(r.Profunctor0())(t)(function(n){return n.value1(n.value0)})(Ps(r)(e))}}},mC=function(t){return function(r){return function(e){return VM(function(n){return new Q(t(n),function(a){return r(n)(a)})})(e)}}};var DC=function(t){return function(){return function(){return function(r){return function(e){return mC(Po(t)()(r))(Tt(ik(t)()()(r)))(e)}}}}};var dC=function(t){return t};var KM=JSON.parse;var YM=JSON.stringify;var Is=function(t){return t};var Rs=function(t){return t};var Ns=function(t){return function(r){return t(r)}},F_=function(t){return{map:function(r){return Ns(p(t)(p(Ec)(r)))}}};var ED=function(t){return{Applicative0:function(){return O_(t)},Bind1:function(){return TD(t)}}},TD=function(t){return{bind:function(r){return function(e){return ct(t.Bind1())(r)(La(function(){var n=f(t.Applicative0());return function(a){return n(Qt.create(a))}}())(function(n){var a=e(n);return a}))}},Apply0:function(){return yC(t)}}},yC=function(t){return{apply:$u(ED(t)),Functor0:function(){return F_(t.Bind1().Apply0().Functor0())}}},O_=function(t){return{pure:function(){var r=f(t.Applicative0());return function(e){return Is(r(Kt.create(e)))}}(),Apply0:function(){return yC(t)}}};var AC=function(t){return{throwError:function(){var r=f(t.Applicative0());return function(e){return Is(r(Qt.create(e)))}}(),Monad0:function(){return ED(t)}}};var SD=function(t){return function(r){return{alt:function(e){return function(n){return ct(r.Bind1())(e)(function(a){if(a instanceof Kt)return f(r.Applicative0())(new Kt(a.value0));if(a instanceof Qt)return ct(r.Bind1())(n)(function(u){if(u instanceof Kt)return f(r.Applicative0())(new Kt(u.value0));if(u instanceof Qt)return f(r.Applicative0())(new Qt(_t(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return F_(r.Bind1().Apply0().Functor0())}}}};var xD=function(){var t=De();return function(r){return t(Rs(r))}}();function $_(t){return Object.prototype.toString.call(t).slice(8,-1)}var gC=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var $D=function(){function t(r,e){this.value0=r,this.value1=e}return t.create=function(r){return function(e){return new t(r,e)}},t}();var CC=ot;var wD=function(t){var r=oo(AC(t));return function(e){return r(bD(e))}};var MD=function(t){return function(r){return function(e){if($_(e)===r)return f(O_(t))(CC(e));if(Qr)return wD(t)(new $D(r,$_(e)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[r.constructor.name,e.constructor.name])}}};var PD=function(t){return MD(t)("String")};var Us=function(){function t(){}return t.value=new t,t}(),Ws=function(){function t(){}return t.value=new t,t}(),TC=function(){function t(){}return t.value=new t,t}(),SC=function(){function t(){}return t.value=new t,t}(),RD=function(){function t(){}return t.value=new t,t}(),xC=function(){function t(){}return t.value=new t,t}(),FC=function(){function t(){}return t.value=new t,t}();var OC=function(t){return t},$C=function(t){return t};var wC=function(t){return t};var MC=function(t){return t};var PC=function(t){return t};var IC=function(t){return t},RC=function(t){return t},NC=function(t){return t},LC=function(t){return t},BC=function(t){return t};var ND=function(){function t(){}return t.value=new t,t}(),UC=function(){function t(){}return t.value=new t,t}(),WC=function(){function t(){}return t.value=new t,t}(),LD=function(){function t(){}return t.value=new t,t}(),qC=function(){function t(){}return t.value=new t,t}();var qs=function(t){return t};var Wf=function(t){return t};var DP=function(t){return t},w_=function(t){return t};var Vc={toAudioOnOff:j(K)};var Gc=function(t){return t.toAudioParameter},HC=function(t){return t.toAudioOnOff},zC=function(){return Cf.create}(),VC=function(){return hf.value}();var Hs=function(){return dC(function(){var t=vC()()(In),r=DC({reflectSymbol:function(){return"o"}})()()(D.value)(vn);return function(e){return t(r(e))}}())},GC=ot;var dP=function(){var t=oe()({reflectSymbol:function(){return"unit"}})(D.value);return function(r){return w_(t(r))}}();var bP=function(t){return function(r){return{toAudioParameter:function(e){return dP(e)}}}},JC=function(t){return function(r){return{toAudioParameter:function(){var e=Gc(bP(t)(r));return function(n){return e(DP(function(a){return{u:a}}(n)))}}()}}},jC=function(){return oe()({reflectSymbol:function(){return"2x"}})(D.value)(void 0)}(),XC=function(){var t=oe()({reflectSymbol:function(){return"sudden"}})(D.value);return function(r){return w_(t(r))}}();var QC={toAudioParameter:XC},zs={toAudioParameter:function(t){return XC({n:t})}},BD=function(){return oe()({reflectSymbol:function(){return"step"}})(D.value)(void 0)}();var UD=function(){return oe()({reflectSymbol:function(){return"on"}})(D.value)(void 0)}(),M_={x:UD,o:0},it=function(){return f(g)(Ye()(oe()({reflectSymbol:function(){return"onOff"}})(D.value)(M_)))};var KC=function(){return oe()({reflectSymbol:function(){return"off"}})(D.value)(void 0)}();var yP=function(){var t=oe()({reflectSymbol:function(){return"numeric"}})(D.value);return function(r){return w_(t(r))}}();var we={toAudioParameter:yP};var Vo=function(){return oe()({reflectSymbol:function(){return"linear"}})(D.value)(void 0)}();var YC=function(){return oe()({reflectSymbol:function(){return"exponential"}})(D.value)(void 0)}(),AP=function(){var t=oe()({reflectSymbol:function(){return"envelope"}})(D.value);return function(r){return w_(t(r))}}();var Sn={toAudioParameter:AP},kP=function(){var t=oe()({reflectSymbol:function(){return"cancel"}})(D.value);return function(r){return w_(t(r))}}();var ZC={toAudioParameter:kP};var gP=function(){function t(){}return t.value=new t,t}(),CP=function(){function t(){}return t.value=new t,t}(),hP=function(){function t(){}return t.value=new t,t}(),EP=function(){function t(){}return t.value=new t,t}(),TP=function(){function t(){}return t.value=new t,t}(),SP=function(){function t(){}return t.value=new t,t}(),xP=function(){function t(){}return t.value=new t,t}(),FP=function(){function t(){}return t.value=new t,t}(),OP=function(){function t(){}return t.value=new t,t}(),$P=function(){function t(){}return t.value=new t,t}(),wP=function(){function t(){}return t.value=new t,t}(),MP=function(){function t(){}return t.value=new t,t}(),PP=function(){function t(){}return t.value=new t,t}(),IP=function(){function t(){}return t.value=new t,t}(),gi=function(t){return{toPeriodicOscSpec:function(r){return oe()({reflectSymbol:function(){return"realImg"}})(D.value)({real:ws(r.value0),img:ws(r.value1)})}}};var Vs={toInitializeTriangleOsc:function(t){return BC(function(r){return{frequency:r}}(t))}};var th={toInitializeStereoPanner:function(t){return LC(function(r){return{pan:r}}(t))}};var qf={toInitializeSquareOsc:function(t){return NC(function(r){return{frequency:r}}(t))}};var cc={toInitializeSinOsc:function(t){return RC(function(r){return{frequency:r}}(t))}};var rh={toInitializeSawtoothOsc:function(t){return IC(function(r){return{frequency:r}}(t))}};var WD={toInitializeRecorder:function(t){return OC(function(r){return{cb:r}}(t))}};var P_={toInitializeMicrophone:function(t){return $C(function(r){return{microphone:r}}(t))}};var eh=function(t){return function(r){return{toInitializeIIRFilter:function(e){return function(n){return function(a){return{feedforward:AD(ki)(wn()(e.value0)),feedback:AD(ki)(wn()(e.value1))}}}}}}};var nt={toInitializeGain:function(t){return PC(function(r){return{gain:r}}(t))}};var nh={toInitializeConvolver:function(t){return wC(function(r){return{buffer:r}}(t))}},Gs={toInitializeConstant:function(t){return MC(function(r){return{offset:r}}(t))}};var RP={convertOption:function(t){return function(r){return j(K)}}},I_={convertOption:function(t){return function(r){return j(K)}}},ah={convertOption:function(t){return function(r){return j(K)}}},uh={convertOption:function(t){return function(r){return T.create}}},oh={convertOption:function(t){return function(r){return j(K)}}},Ci={convertOption:function(t){return function(r){return j(K)}}},Hf={convertOption:function(t){return function(r){return j(K)}}},zf={convertOption:function(t){return function(r){return j(K)}}},Vf={convertOption:function(t){return function(r){return j(K)}}},Gf={convertOption:function(t){return function(r){return j(K)}}},Jf={convertOption:function(t){return function(r){return j(K)}}},ih={convertOption:function(t){return function(r){return j(K)}}},ch={convertOption:function(t){return function(r){return j(K)}}},fh={convertOption:function(t){return function(r){return j(K)}}},qD={convertOption:function(t){return function(r){return j(K)}}},Jc={convertOption:function(t){return function(r){return j(K)}}},R_={convertOption:function(t){return function(r){return j(K)}}},N_={convertOption:function(t){return function(r){return j(K)}}};var jf={convertOption:function(t){return function(r){return j(K)}}},lh={convertOption:function(t){return function(r){return j(K)}}},_h={convertOption:function(t){return function(r){return j(K)}}},ph={convertOption:function(t){return function(r){return j(K)}}},HD={convertOption:function(t){return function(r){return j(K)}}};var sh={convertOption:function(t){return function(r){return j(K)}}},zD={convertOption:function(t){return function(r){return j(K)}}},mn={convertOption:function(t){return function(r){return j(K)}}},en={convertOption:function(t){return function(r){return j(K)}}},VD={convertOption:function(t){return function(r){return j(K)}}},Js={convertOption:function(t){return function(r){return j(K)}}},NP=function(t){return t.toPeriodicOscSpec},hi=function(t){return{convertOption:function(r){return function(e){return NP(t)}}}},GD=function(t){return t.toInitializeWaveShaper},vh=function(t){return t.toInitializeTriangleOsc},mh=function(t){return t.toInitializeStereoPanner},Dh=function(t){return t.toInitializeSquareOsc},dh=function(t){return t.toInitializeSinOsc},bh=function(t){return t.toInitializeSawtoothOsc},yh=function(t){return t.toInitializeRecorder},JD=function(t){return t.toInitializePlayBuf},Ah=function(t){return t.toInitializePeriodicOsc},kh=function(t){return t.toInitializePeaking},gh=function(t){return t.toInitializeNotch},Ch=function(t){return t.toInitializeMicrophone},hh=function(t){return t.toInitializeLowshelf},jD=function(t){return t.toInitializeLowpass},XD=function(t){return t.toInitializeLoopBuf},Eh=function(t){return t.toInitializeIIRFilter},Th=function(t){return t.toInitializeHighshelf},QD=function(t){return t.toInitializeHighpass},Sh=function(t){return t.toInitializeGain},xh=function(t){return t.toInitializeDynamicsCompressor},KD=function(t){return t.toInitializeDelay},Fh=function(t){return t.toInitializeConvolver},Oh=function(t){return t.toInitializeConstant},YD=function(t){return t.toInitializeBandpass},ZD=function(t){return t.toInitializeAllpass};var LP={oversample:jC},BP=function(t){return{toInitializeWaveShaper:function(r){return _a(t)(gP.value)(LP)(r)}}},$h={toInitializeWaveShaper:function(){var t=GD(BP(bt(dt()(q(Dt)(RP)()()()({reflectSymbol:function(){return"curve"}})))(mt()())));return function(r){return t(function(e){return{curve:e}}(r))}}()},UP=function(){return{bufferOffset:0,playbackRate:1,duration:L.value}}(),L_=function(t){return{toInitializePlayBuf:function(r){return _a(t)(CP.value)(UP)(r)}}},Va={toInitializePlayBuf:function(){var t=JD(L_(bt(dt()(q(Dt)(I_)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},WP={},Ei=function(t){return{toInitializePeriodicOsc:function(r){return _a(t)(hP.value)(WP)(r)}}},qP={q:1,gain:0},Xf=function(t){return{toInitializePeaking:function(r){return _a(t)(EP.value)(qP)(r)}}};var HP={q:1},Qf=function(t){return{toInitializeNotch:function(r){return _a(t)(TP.value)(HP)(r)}}};var zP={gain:0},wh=function(t){return{toInitializeLowshelf:function(r){return _a(t)(SP.value)(zP)(r)}}};var VP={q:1},td=function(t){return{toInitializeLowpass:function(r){return _a(t)(xP.value)(VP)(r)}}},js={toInitializeLowpass:function(){var t=jD(td(bt(dt()(q(Dt)(qD)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},GP=function(){return{loopStart:0,loopEnd:0,playbackRate:1,duration:L.value}}(),jc=function(t){return{toInitializeLoopBuf:function(r){return _a(t)(FP.value)(GP)(r)}}},br={toInitializeLoopBuf:function(){var t=XD(jc(bt(dt()(q(Dt)(Jc)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())));return function(r){return t(function(e){return{buffer:e}}(r))}}()},JP={gain:0},Mh=function(t){return{toInitializeHighshelf:function(r){return _a(t)(OP.value)(JP)(r)}}};var jP={q:1},rd=function(t){return{toInitializeHighpass:function(r){return _a(t)($P.value)(jP)(r)}}},nu={toInitializeHighpass:function(){var t=QD(rd(bt(dt()(q(Dt)(HD)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},XP=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),Ph=function(t){return{toInitializeDynamicsCompressor:function(r){return _a(t)(wP.value)(XP)(r)}}},QP={maxDelayTime:1},ed=function(t){return{toInitializeDelay:function(r){return _a(t)(MP.value)(QP)(r)}}},Je={toInitializeDelay:function(){var t=KD(ed(bt(dt()(q(Dt)(zD)()()()({reflectSymbol:function(){return"delayTime"}})))(mt()())));return function(r){return t(function(e){return{delayTime:e}}(r))}}()},KP={q:1},nn=function(t){return{toInitializeBandpass:function(r){return _a(t)(PP.value)(KP)(r)}}},nd={toInitializeBandpass:function(){var t=YD(nn(bt(dt()(q(Dt)(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()},YP={q:1},Xs=function(t){return{toInitializeAllpass:function(r){return _a(t)(IP.value)(YP)(r)}}},ad={toInitializeAllpass:function(){var t=ZD(Xs(bt(dt()(q(Dt)(Js)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())));return function(r){return t(function(e){return{frequency:e}}(r))}}()};var rI=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var eI=function(){function t(){}return t.value=new t,t}();var Qs={convertOption:function(t){return function(r){return j(K)}}},Ks={convertOption:function(t){return function(r){return j(K)}}};var nI=function(t){return t.toInitializeAnalyser},Se=function(t){if(t instanceof a_)return L.value;if(t instanceof Io)return new T(t.value0);throw new Error("Failed pattern match at Ocarina.Control (line 36, column 1 - line 36, column 38): "+[t.constructor.name])},au=Fk({doLogic:Ac,ids:function(){var t=De();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}})({fromElt:wn(),connectToParent:function(t){return function(r){return t.connectXToY({from:r.id,to:r.parent})}}});var aI=function(){return{cb:function(t){return f(ft)(f(ft)(void 0))},fftSize:RD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:LD.value,channelInterpretation:ND.value}}(),Ys=function(t){return{toInitializeAnalyser:function(r){return _a(t)(eI.value)(aI)(r)}}};var uI=function(t){return function(r){var e=Ch(t)(r),n=function(a){return function(u){return Er(function(o){return function(i){return function(){var m=u.ids();return a.raiseId(m)(),p(rr)(function(l){return X(ae)(i(u.deleteFromCache({id:m})))(l)})(Tt(o)(i)(f(g)(u.makeMicrophone({id:m,parent:a.parent,scope:Se(a.scope),microphone:e.microphone}))))()}}})}};return new F(n)}},B_=function(t){return uI(t)};var Wn=Uu({doLogic:Ac,ids:function(){var t=De();return function(r){return function(e){return e.ids}(t(r))}}(),disconnectElement:function(t){return function(r){return t.disconnectXFromY({from:r.id,to:r.parent})}},toElt:function(t){return t}}),oI=function(t){return function(r){return function(e){return function(n){var a=nI(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeAnalyser({id:s,parent:o.parent,scope:Se(o.scope),cb:a.cb,fftSize:jm(2)(function(){if(a.fftSize instanceof Us)return 7;if(a.fftSize instanceof Ws)return 8;if(a.fftSize instanceof TC)return 9;if(a.fftSize instanceof SC)return 10;if(a.fftSize instanceof RD)return 11;if(a.fftSize instanceof xC)return 12;if(a.fftSize instanceof FC)return 13;throw new Error("Failed pattern match at Ocarina.Control (line 190, column 21 - line 197, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof qC)return"explicit";if(a.channelCountMode instanceof LD)return"max";if(a.channelCountMode instanceof WC)return"clamped-max";throw new Error("Failed pattern match at Ocarina.Control (line 203, column 35 - line 206, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof ND)return"speakers";if(a.channelInterpretation instanceof UC)return"discrete";throw new Error("Failed pattern match at Ocarina.Control (line 207, column 40 - line 209, column 41): "+[a.channelInterpretation.constructor.name])}()})))(O($)(p(k)(function(v){return Ne()()()({cb:function(c){return i.setAnalyserNodeCb({id:s,cb:c})}})(v)})(e))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Zs=function(t){return function(r){return oI(t)(r)(x(h))}},Ih=function(t){return function(r){return function(e){var n=Fh(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeConvolver({id:l,parent:u.parent,scope:Se(u.scope),buffer:n.buffer})))(Wn({parent:new T(l),scope:u.scope,raiseId:function(s){return f(sr)(void 0)}})(o)(N(e)))))()}}})}};return new F(a)}}},iI=function(){return function(){return function(t){return function(r){return function(e){return function(n){return function(a){var u=Eh(t)(n)(r)(e),o=function(i){return function(_){return Er(function(m){return function(l){return function(){var v=_.ids();return i.raiseId(v)(),p(rr)(function(c){return X(ae)(l(_.deleteFromCache({id:v})))(c)})(Tt(m)(l)(O($)(f(g)(_.makeIIRFilter({id:v,parent:i.parent,scope:Se(i.scope),feedforward:Nc()(u.feedforward),feedback:Nc()(u.feedback)})))(Wn({parent:new T(v),scope:i.scope,raiseId:function(c){return f(sr)(void 0)}})(_)(N(a)))))()}}})}};return new F(o)}}}}}}},Rh=function(){return function(){return function(t){return iI()()(t)(D.value)(D.value)}}},ud=function(t){return function(r){return function(e){var n=yh(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeRecorder({id:l,parent:u.parent,scope:Se(u.scope),cb:n.cb})))(Wn({parent:new T(l),scope:u.scope,raiseId:function(s){return f(sr)(void 0)}})(o)(e))))()}}})}};return new F(a)}}},cI=function(t){return function(r){return Er(function(e){return function(n){return function(){var u=r.ids();return n(r.makeSpeaker({id:u}))(),e(Wn({parent:new T(u),scope:new Io("toplevel"),raiseId:function(o){return f(sr)(void 0)}})(r)(N(t)))(n)()}}})}},Xc=cI,Ft=function(t){return function(r){return function(e){return Le(t)(r)(x(h))(e)}}},Le=function(t){return function(r){return function(e){return function(n){var a=Sh(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeGain({id:s,parent:o.parent,scope:Se(o.scope),gain:a.gain})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({gain:Nh(592)(o.scope)(i)(function(c){return i.setGain(function(d){return{id:s,gain:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Nh=rI("tmpResolveAU","Ocarina.Control",function(){var t=function(){var o=oe()({reflectSymbol:function(){return"unit"}})(D.value);return function(i){return Wf(o(i))}}(),r=function(){var o=oe()({reflectSymbol:function(){return"sudden"}})(D.value);return function(i){return Wf(o(i))}}(),e=function(){var o=oe()({reflectSymbol:function(){return"numeric"}})(D.value);return function(i){return Wf(o(i))}}(),n=function(){var o=oe()({reflectSymbol:function(){return"envelope"}})(D.value);return function(i){return Wf(o(i))}}(),a=function(){var o=oe()({reflectSymbol:function(){return"cancel"}})(D.value);return function(i){return Wf(o(i))}}(),u=function(o){return function(i){return function(_){return function(m){return Ne()()()({numeric:function(){var l=f(g);return function(s){return l(_(e(s)))}}(),envelope:function(){var l=f(g);return function(s){return l(_(n(s)))}}(),cancel:function(){var l=f(g);return function(s){return l(_(a(s)))}}(),sudden:function(){var l=f(g);return function(s){return l(_(r(s)))}}(),unit:function(l){var s=Ft(nt)(1)([l.u]);return Er(function(v){return function(c){return function(){var rt=Oe(L.value)();return v(O($)(Wn({parent:L.value,scope:o,raiseId:function(Z){return tr(rr)(mu(new T(Z))(rt))}})(i)(s))(Er(function(Z){return function(Vt){return function(){return function(){var ie=Qe(rt)();if(ie instanceof L)return void 0;if(ie instanceof T)return Vt(_(t({i:ie.value0})))();throw new Error("Failed pattern match at Ocarina.Control (line 1675, column 36 - line 1677, column 65): "+[ie.constructor.name])}(),f(sr)(void 0)}}})))(c)()}}})}})(m)}}}};return u}),re=Nh(1654),fI=function(t){return function(r){return function(e){var n=XD(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeLoopBuf({id:l,parent:u.parent,scope:Se(u.scope),buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(me(Pt)(p(k)(function(s){return Ne()()()({buffer:function(v){return f(g)(o.setBuffer({id:l,buffer:v}))},playbackRate:re(u.scope)(o)(function(v){return o.setPlaybackRate(function(c){return{id:l,playbackRate:c}}(v))}),loopStart:function(v){return f(g)(o.setLoopStart({id:l,loopStart:v}))},loopEnd:function(v){return f(g)(o.setLoopEnd({id:l,loopEnd:v}))},onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},ar=function(t){return fI(t)};var lI=function(t){return function(r){return function(e){var n=Ah(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makePeriodicOsc({id:l,parent:u.parent,scope:Se(u.scope),frequency:n.frequency,spec:n.spec})))(me(Pt)(p(k)(function(s){return Ne()()()({frequency:re(u.scope)(o)(function(v){return o.setFrequency(function(c){return{id:l,frequency:c}}(v))}),onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))},spec:function(v){return f(g)(o.setPeriodicOsc({id:l,spec:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},Ti=function(t){return lI(t)};var _I=function(t){return function(r){return function(e){var n=JD(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makePlayBuf({id:l,parent:u.parent,scope:Se(u.scope),buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(me(Pt)(p(k)(function(s){return Ne()()()({buffer:function(v){return f(g)(o.setBuffer({id:l,buffer:v}))},playbackRate:re(u.scope)(o)(function(v){return o.setPlaybackRate(function(c){return{id:l,playbackRate:c}}(v))}),bufferOffset:function(v){return f(g)(o.setBufferOffset({id:l,bufferOffset:v}))},onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))},duration:function(v){return f(g)(o.setDuration({id:l,duration:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},qn=function(t){return _I(t)};var pI=function(t){return function(r){return function(e){var n=bh(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeSawtoothOsc({id:l,parent:u.parent,scope:Se(u.scope),frequency:n.frequency})))(me(Pt)(p(k)(function(s){return Ne()()()({frequency:re(u.scope)(o)(function(v){return o.setFrequency(function(c){return{id:l,frequency:c}}(v))}),onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},Lh=function(t){return pI(t)};var sI=function(t){return function(r){return function(e){var n=dh(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeSinOsc({id:l,parent:u.parent,scope:Se(u.scope),frequency:n.frequency})))(me(Pt)(p(k)(function(s){return Ne()()()({frequency:re(u.scope)(o)(function(v){return o.setFrequency(function(c){return{id:l,frequency:c}}(v))}),onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},Qc=function(t){return sI(t)},Bh=function(t){return function(r){return Qc(t)(r)(x(h))}},vI=function(t){return function(r){return function(e){var n=Dh(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeSquareOsc({id:l,parent:u.parent,scope:Se(u.scope),frequency:n.frequency})))(me(Pt)(p(k)(function(s){return Ne()()()({frequency:re(u.scope)(o)(function(v){return o.setFrequency(function(c){return{id:l,frequency:c}}(v))}),onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},U_=function(t){return vI(t)},Uh=function(t){return function(r){return U_(t)(r)(x(h))}},mI=function(t){return function(r){return function(e){var n=vh(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeTriangleOsc({id:l,parent:u.parent,scope:Se(u.scope),frequency:n.frequency})))(me(Pt)(p(k)(function(s){return Ne()()()({frequency:re(u.scope)(o)(function(v){return o.setFrequency(function(c){return{id:l,frequency:c}}(v))}),onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},tv=function(t){return mI(t)};var DI=function(t){return function(r){return function(e){return function(n){var a=ZD(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeAllpass({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,q:a.q})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),q:re(o.scope)(i)(function(c){return i.setQ(function(d){return{id:s,q:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},W_=function(t){return function(r){return function(e){return DI(t)(r)(x(h))(e)}}},od=function(t){return function(r){return function(e){return function(n){var a=YD(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeBandpass({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,q:a.q})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),q:re(o.scope)(i)(function(c){return i.setQ(function(d){return{id:s,q:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Dn=function(t){return function(r){return function(e){return od(t)(r)(x(h))(e)}}},q_=function(t){return function(r){return function(e){return function(n){var a=KD(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeDelay({id:s,parent:o.parent,scope:Se(o.scope),delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({delayTime:re(o.scope)(i)(function(c){return i.setDelay(function(d){return{id:s,delayTime:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Do=function(t){return function(r){return function(e){return q_(t)(r)(x(h))(e)}}},dI=function(t){return function(r){return function(e){return function(n){var a=xh(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeDynamicsCompressor({id:s,parent:o.parent,scope:Se(o.scope),threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({threshold:re(o.scope)(i)(function(c){return i.setThreshold(function(d){return{id:s,threshold:d}}(c))}),ratio:re(o.scope)(i)(function(c){return i.setRatio(function(d){return{id:s,ratio:d}}(c))}),knee:re(o.scope)(i)(function(c){return i.setKnee(function(d){return{id:s,knee:d}}(c))}),attack:re(o.scope)(i)(function(c){return i.setAttack(function(d){return{id:s,attack:d}}(c))}),release:re(o.scope)(i)(function(c){return i.setRelease(function(d){return{id:s,release:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Wh=function(t){return function(r){return dI(t)(r)(x(h))}},bI=function(){return function(t){return function(r){return qm()({doLogic:Ac,ids:function(){var e=De();return function(n){return function(a){return a.ids}(e(n))}}(),disconnectElement:function(e){return function(n){return e.disconnectXFromY({from:n.id,to:n.parent})}},toElt:function(e){return e}})({fromEltO1:wn(),fromEltO2:wn(),toElt:wn(),wrapElt:function(e){return Ft(nt)(1)([e])},giveNewParent:function(e){return function(n){return function(a){return e.connectXToY({from:n.id,to:n.parent})}}},deleteFromCache:function(){var e=De();return function(n){return function(a){return a.deleteFromCache}(e(n))}}()})(t)(qa(In)(p(o_)(function(e){return e(void 0)}))(wn()(r)))}}},Oa=function(t){return function(r){return bI()(Dk(t))(qa(In)(dk()()()()()({reflectType:function(){return 0}})(D.value))(r))}};var id=function(t){return function(r){return function(e){return function(n){var a=QD(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeHighpass({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,q:a.q})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),q:re(o.scope)(i)(function(c){return i.setQ(function(d){return{id:s,q:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Kf=function(t){return function(r){return function(e){return id(t)(r)(x(h))(e)}}},yI=function(t){return function(r){return function(e){return function(n){var a=Th(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeHighshelf({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,gain:a.gain})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),gain:re(o.scope)(i)(function(c){return i.setGain(function(d){return{id:s,gain:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},qh=function(t){return function(r){return function(e){return yI(t)(r)(x(h))(e)}}},Hh=function(t){return function(r){return function(e){return function(n){var a=jD(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeLowpass({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,q:a.q})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),q:re(o.scope)(i)(function(c){return i.setQ(function(d){return{id:s,q:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Yf=function(t){return function(r){return function(e){return Hh(t)(r)(x(h))(e)}}},AI=function(t){return function(r){return function(e){return function(n){var a=hh(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeLowshelf({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,gain:a.gain})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),gain:re(o.scope)(i)(function(c){return i.setGain(function(d){return{id:s,gain:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},zh=function(t){return function(r){return function(e){return AI(t)(r)(x(h))(e)}}},kI=function(t){return function(r){return function(e){return function(n){var a=gh(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeNotch({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,q:a.q})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),q:re(o.scope)(i)(function(c){return i.setQ(function(d){return{id:s,q:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Zf=function(t){return function(r){return function(e){return kI(t)(r)(x(h))(e)}}},gI=function(t){return function(r){return function(e){return function(n){var a=mh(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makeStereoPanner({id:s,parent:o.parent,scope:Se(o.scope),pan:a.pan})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({pan:re(o.scope)(i)(function(c){return i.setPan(function(d){return{id:s,pan:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},Vh=function(t){return function(r){return gI(t)(r)(x(h))}},CI=function(t){return function(r){return function(e){return function(n){var a=kh(t)(r),u=function(o){return function(i){return Er(function(_){return function(m){return function(){var s=i.ids();return o.raiseId(s)(),p(rr)(function(v){return X(ae)(m(i.deleteFromCache({id:s})))(v)})(Tt(_)(m)(O($)(f(g)(i.makePeaking({id:s,parent:o.parent,scope:Se(o.scope),frequency:a.frequency,q:a.q,gain:a.gain})))(O($)(me(Pt)(p(k)(function(v){return Ne()()()({frequency:re(o.scope)(i)(function(c){return i.setFrequency(function(d){return{id:s,frequency:d}}(c))}),q:re(o.scope)(i)(function(c){return i.setQ(function(d){return{id:s,q:d}}(c))}),gain:re(o.scope)(i)(function(c){return i.setGain(function(d){return{id:s,gain:d}}(c))})})(v)})(e)))(Wn({parent:new T(s),scope:o.scope,raiseId:function(v){return f(sr)(void 0)}})(i)(N(n))))))()}}})}};return new F(u)}}}},tl=function(t){return function(r){return function(e){return CI(t)(r)(x(h))(e)}}},Gh=function(t){return function(r){return function(e){var n=GD(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeWaveShaper({id:l,parent:u.parent,scope:Se(u.scope),curve:n.curve,oversample:n.oversample})))(Wn({parent:new T(l),scope:u.scope,raiseId:function(s){return f(sr)(void 0)}})(o)(N(e)))))()}}})}};return new F(a)}}},hI=function(t){return function(r){return function(e){var n=Oh(t)(r),a=function(u){return function(o){return Er(function(i){return function(_){return function(){var l=o.ids();return u.raiseId(l)(),p(rr)(function(s){return X(ae)(_(o.deleteFromCache({id:l})))(s)})(Tt(i)(_)(O($)(f(g)(o.makeConstant({id:l,parent:u.parent,scope:Se(u.scope),offset:n.offset})))(me(Pt)(p(k)(function(s){return Ne()()()({offset:re(u.scope)(o)(function(v){return o.setOffset(function(c){return{id:l,offset:c}}(v))}),onOff:function(v){return f(g)(o.setOnOff({id:l,onOff:v}))}})(s)})(e)))))()}}})}};return new F(a)}}},rv=function(t){return hI(t)};function cd(){window.scrollTo(0,0)}var bo=function(t){return t.sequential},xn=function(t){return t.parallel};var dn=function(t){return function(r){return new F(z("button")(t)(N(r)))}};var $a=function(){var t={},r="Pure",e="Throw",n="Catch",a="Sync",u="Async",o="Bind",i="Bracket",_="Fork",m="Sequential",l="Map",s="Apply",v="Alt",c="Cons",d="Resume",rt="Release",Z="Finalizer",Vt="Finalized",zt="Forked",Jr="Fiber",ie="Thunk";function ut(Rt,zr,We,ne){this.tag=Rt,this._1=zr,this._2=We,this._3=ne}function It(Rt){var zr=function(We,ne,er){return new ut(Rt,We,ne,er)};return zr.tag=Rt,zr}function Cr(Rt){return new ut(r,void 0)}function Et(Rt){try{Rt()}catch(zr){setTimeout(function(){throw zr},0)}}function ee(Rt,zr,We){try{return zr(We())}catch(ne){return Rt(ne)}}function Hn(Rt,zr,We){try{return zr(We)()}catch(ne){return We(Rt(ne))(),Cr}}var Be=function(){var Rt=1024,zr=0,We=0,ne=new Array(Rt),er=!1;function kt(){var Xr;for(er=!0;zr!==0;)zr--,Xr=ne[We],ne[We]=void 0,We=(We+1)%Rt,Xr();er=!1}return{isDraining:function(){return er},enqueue:function(Xr){var Or,Me;zr===Rt&&(Me=er,kt(),er=Me),ne[(We+zr)%Rt]=Xr,zr++,er||kt()}}}();function ya(Rt){var zr={},We=0,ne=0;return{register:function(er){var kt=We++;er.onComplete({rethrow:!0,handler:function(Xr){return function(){ne--,delete zr[kt]}}})(),zr[kt]=er,ne++},isEmpty:function(){return ne===0},killAll:function(er,kt){return function(){if(ne===0)return kt();var Xr=0,Or={};function Me(_e){Or[_e]=zr[_e].kill(er,function(je){return function(){delete Or[_e],Xr--,Rt.isLeft(je)&&Rt.fromLeft(je)&&setTimeout(function(){throw Rt.fromLeft(je)},0),Xr===0&&kt()}})()}for(var Vn in zr)zr.hasOwnProperty(Vn)&&(Xr++,Me(Vn));return zr={},We=0,ne=0,function(_e){return new ut(a,function(){for(var je in Or)Or.hasOwnProperty(je)&&Or[je]()})}}}}}var ua=0,Ue=1,Ju=2,Yo=3,ef=4,Ia=5,zn=6;function bc(Rt,zr,We){var ne=0,er=ua,kt=We,Xr=null,Or=null,Me=null,Vn=null,_e=null,je=0,yc=0,Fu=null,Ni=!0;function Li(nr){for(var ur,Br,qr;;)switch(ur=null,Br=null,qr=null,er){case Ju:er=Ue;try{kt=Me(kt),Vn===null?Me=null:(Me=Vn._1,Vn=Vn._2)}catch(oa){er=Ia,Xr=Rt.left(oa),kt=null}break;case Yo:Rt.isLeft(kt)?(er=Ia,Xr=kt,kt=null):Me===null?er=Ia:(er=Ju,kt=Rt.fromRight(kt));break;case Ue:switch(kt.tag){case o:Me&&(Vn=new ut(c,Me,Vn)),Me=kt._2,er=Ue,kt=kt._1;break;case r:Me===null?(er=Ia,kt=Rt.right(kt._1)):(er=Ju,kt=kt._1);break;case a:er=Yo,kt=ee(Rt.left,Rt.right,kt._1);break;case u:er=ef,kt=Hn(Rt.left,kt._1,function(oa){return function(){ne===nr&&(ne++,Be.enqueue(function(){ne===nr+1&&(er=Yo,kt=oa,Li(ne))}))}});return;case e:er=Ia,Xr=Rt.left(kt._1),kt=null;break;case n:Me===null?_e=new ut(c,kt,_e,Or):_e=new ut(c,kt,new ut(c,new ut(d,Me,Vn),_e,Or),Or),Me=null,Vn=null,er=Ue,kt=kt._1;break;case i:je++,Me===null?_e=new ut(c,kt,_e,Or):_e=new ut(c,kt,new ut(c,new ut(d,Me,Vn),_e,Or),Or),Me=null,Vn=null,er=Ue,kt=kt._1;break;case _:er=Yo,ur=bc(Rt,zr,kt._2),zr&&zr.register(ur),kt._1&&ur.run(),kt=Rt.right(ur);break;case m:er=Ue,kt=Zo(Rt,zr,kt._1);break}break;case Ia:if(Me=null,Vn=null,_e===null)er=zn,kt=Or||Xr||kt;else switch(ur=_e._3,qr=_e._1,_e=_e._2,qr.tag){case n:Or&&Or!==ur&&je===0?er=Ia:Xr&&(er=Ue,kt=qr._2(Rt.fromLeft(Xr)),Xr=null);break;case d:Or&&Or!==ur&&je===0||Xr?er=Ia:(Me=qr._1,Vn=qr._2,er=Ju,kt=Rt.fromRight(kt));break;case i:je--,Xr===null&&(Br=Rt.fromRight(kt),_e=new ut(c,new ut(rt,qr._2,Br),_e,ur),(Or===ur||je>0)&&(er=Ue,kt=qr._3(Br)));break;case rt:_e=new ut(c,new ut(Vt,kt,Xr),_e,Or),er=Ue,Or&&Or!==ur&&je===0?kt=qr._1.killed(Rt.fromLeft(Or))(qr._2):Xr?kt=qr._1.failed(Rt.fromLeft(Xr))(qr._2):kt=qr._1.completed(Rt.fromRight(kt))(qr._2),Xr=null,je++;break;case Z:je++,_e=new ut(c,new ut(Vt,kt,Xr),_e,Or),er=Ue,kt=qr._1;break;case Vt:je--,er=Ia,kt=qr._1,Xr=qr._2;break}break;case zn:for(var He in Fu)Fu.hasOwnProperty(He)&&(Ni=Ni&&Fu[He].rethrow,Et(Fu[He].handler(kt)));Fu=null,Or&&Xr?setTimeout(function(){throw Rt.fromLeft(Xr)},0):Rt.isLeft(kt)&&Ni&&setTimeout(function(){if(Ni)throw Rt.fromLeft(kt)},0);return;case ua:er=Ue;break;case ef:return}}function qe(nr){return function(){if(er===zn)return Ni=Ni&&nr.rethrow,nr.handler(kt)(),function(){};var ur=yc++;return Fu=Fu||{},Fu[ur]=nr,function(){Fu!==null&&delete Fu[ur]}}}function lr(nr,ur){return function(){if(er===zn)return ur(Rt.right(void 0))(),function(){};var Br=qe({rethrow:!1,handler:function(){return ur(Rt.right(void 0))}})();switch(er){case ua:Or=Rt.left(nr),er=zn,kt=Or,Li(ne);break;case ef:Or===null&&(Or=Rt.left(nr)),je===0&&(er===ef&&(_e=new ut(c,new ut(Z,kt(nr)),_e,Or)),er=Ia,kt=null,Xr=null,Li(++ne));break;default:Or===null&&(Or=Rt.left(nr)),je===0&&(er=Ia,kt=null,Xr=null)}return Br}}function wr(nr){return function(){var ur=qe({rethrow:!1,handler:nr})();return er===ua&&Li(ne),ur}}return{kill:lr,join:wr,onComplete:qe,isSuspended:function(){return er===ua},run:function(){er===ua&&(Be.isDraining()?Li(ne):Be.enqueue(function(){Li(ne)}))}}}function cp(Rt,zr,We,ne){var er=0,kt={},Xr=0,Or={},Me=new Error("[ParAff] Early exit"),Vn=null,_e=t;function je(qe,lr,wr){var nr=lr,ur=null,Br=null,qr=0,He={},oa,Al;t:for(;;)switch(oa=null,nr.tag){case zt:if(nr._3===t&&(oa=kt[nr._1],He[qr++]=oa.kill(qe,function(LT){return function(){qr--,qr===0&&wr(LT)()}})),ur===null)break t;nr=ur._2,Br===null?ur=null:(ur=Br._1,Br=Br._2);break;case l:nr=nr._2;break;case s:case v:ur&&(Br=new ut(c,ur,Br)),ur=nr,nr=nr._1;break}if(qr===0)wr(Rt.right(void 0))();else for(Al=0,oa=qr;Al<oa;Al++)He[Al]=He[Al]();return He}function yc(qe,lr,wr){var nr,ur,Br,qr,He,oa;Rt.isLeft(qe)?(nr=qe,ur=null):(ur=qe,nr=null);t:for(;;){if(Br=null,qr=null,He=null,oa=null,Vn!==null)return;if(lr===null){ne(nr||ur)();return}if(lr._3!==t)return;switch(lr.tag){case l:nr===null?(lr._3=Rt.right(lr._1(Rt.fromRight(ur))),ur=lr._3):lr._3=nr;break;case s:if(Br=lr._1._3,qr=lr._2._3,nr){if(lr._3=nr,He=!0,oa=Xr++,Or[oa]=je(Me,nr===Br?lr._2:lr._1,function(){return function(){delete Or[oa],He?He=!1:wr===null?yc(nr,null,null):yc(nr,wr._1,wr._2)}}),He){He=!1;return}}else{if(Br===t||qr===t)return;ur=Rt.right(Rt.fromRight(Br)(Rt.fromRight(qr))),lr._3=ur}break;case v:if(Br=lr._1._3,qr=lr._2._3,Br===t&&Rt.isLeft(qr)||qr===t&&Rt.isLeft(Br))return;if(Br!==t&&Rt.isLeft(Br)&&qr!==t&&Rt.isLeft(qr))nr=ur===Br?qr:Br,ur=null,lr._3=nr;else if(lr._3=ur,He=!0,oa=Xr++,Or[oa]=je(Me,ur===Br?lr._2:lr._1,function(){return function(){delete Or[oa],He?He=!1:wr===null?yc(ur,null,null):yc(ur,wr._1,wr._2)}}),He){He=!1;return}break}wr===null?lr=null:(lr=wr._1,wr=wr._2)}}function Fu(qe){return function(lr){return function(){delete kt[qe._1],qe._3=lr,yc(lr,qe._2._1,qe._2._2)}}}function Ni(){var qe=Ue,lr=We,wr=null,nr=null,ur,Br;t:for(;;)switch(ur=null,Br=null,qe){case Ue:switch(lr.tag){case l:wr&&(nr=new ut(c,wr,nr)),wr=new ut(l,lr._1,t,t),lr=lr._2;break;case s:wr&&(nr=new ut(c,wr,nr)),wr=new ut(s,t,lr._2,t),lr=lr._1;break;case v:wr&&(nr=new ut(c,wr,nr)),wr=new ut(v,t,lr._2,t),lr=lr._1;break;default:Br=er++,qe=Ia,ur=lr,lr=new ut(zt,Br,new ut(c,wr,nr),t),ur=bc(Rt,zr,ur),ur.onComplete({rethrow:!1,handler:Fu(lr)})(),kt[Br]=ur,zr&&zr.register(ur)}break;case Ia:if(wr===null)break t;wr._1===t?(wr._1=lr,qe=Ue,lr=wr._2,wr._2=t):(wr._2=lr,lr=wr,nr===null?wr=null:(wr=nr._1,nr=nr._2))}for(_e=lr,Br=0;Br<er;Br++)kt[Br].run()}function Li(qe,lr){Vn=Rt.left(qe);var wr;for(var nr in Or)if(Or.hasOwnProperty(nr)){wr=Or[nr];for(nr in wr)wr.hasOwnProperty(nr)&&wr[nr]()}Or=null;var ur=je(qe,_e,lr);return function(Br){return new ut(u,function(qr){return function(){for(var He in ur)ur.hasOwnProperty(He)&&ur[He]();return Cr}})}}return Ni(),function(qe){return new ut(u,function(lr){return function(){return Li(qe,lr)}})}}function Zo(Rt,zr,We){return new ut(u,function(ne){return function(){return cp(Rt,zr,We,ne)}})}return ut.EMPTY=t,ut.Pure=It(r),ut.Throw=It(e),ut.Catch=It(n),ut.Sync=It(a),ut.Async=It(u),ut.Bind=It(o),ut.Bracket=It(i),ut.Fork=It(_),ut.Seq=It(m),ut.ParMap=It(l),ut.ParApply=It(s),ut.ParAlt=It(v),ut.Fiber=bc,ut.Supervisor=ya,ut.Scheduler=Be,ut.nonCanceler=Cr,ut}(),Jh=$a.Pure,wI=$a.Throw;function jh(t){return function(r){return r.tag===$a.Pure.tag?$a.Pure(t(r._1)):$a.Bind(r,function(e){return $a.Pure(t(e))})}}function Xh(t){return function(r){return $a.Bind(t,r)}}var Qh=$a.Sync;function Kh(t){return function(r){return $a.ParMap(t,r)}}function Yh(t){return function(r){return $a.ParApply(t,r)}}function Zh(t){return function(r){return $a.ParAlt(t,r)}}var rl=$a.Async;function tE(t,r){return function(){return $a.Fiber(t,null,r)}}var MI=function(){function t(e,n){return e===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,e)}function r(e,n){return e===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(e,n){return $a.Async(function(a){return function(){var u=t(n,a(e()));return function(){return $a.Sync(function(){return e(r(n,u))})}}})}}(),rE=$a.Seq;var II=function(t){return function(r){return function(e){var n=bo(t),a=$e(t.Applicative1())(r)(function(){var u=xn(t);return function(o){return u(e(o))}}());return function(u){return n(a(u))}}}},eE=function(t){return function(r){return function(e){var n=bo(t),a=Mn(r)(t.Applicative1())(function(){var u=xn(t);return function(o){return u(e(o))}}());return function(u){return n(a(u))}}}},nE=function(t){return function(r){return II(t)(r)(j(K))}};var RI=function(t){return t};var uE=function(t){return t};var z_=function(t){return t.toDuration};var oE={fromDuration:Xv()()(RI)(function(t){return t*1e3}),toDuration:Xv()()(uE)(function(t){return t/1e3})};var iE=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}};var LI=function(t){return t};var nl={map:Kh},Si={map:jh};var BI=function(){var t=function(n){if(n instanceof Kt)return n.value0;if(n instanceof Qt)return du("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},r=function(n){if(n instanceof Qt)return n.value0;if(n instanceof Kt)return du("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},e=function(n){if(n instanceof Qt)return!0;if(n instanceof Kt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:e,fromLeft:r,fromRight:t,left:Qt.create,right:Kt.create}}(),UI=function(t){return tE(BI,t)},yo=function(t){return function(){var e=UI(t)();return e.run(),e}},Jo=function(){var t=tr(R);return function(r){return t(yo(r))}}();var xi={apply:Yh,Functor0:function(){return nl}};var fd={Applicative0:function(){return da},Bind1:function(){return Fe}},Fe={bind:Xh,Apply0:function(){return ld(0)}},da={pure:Jh,Apply0:function(){return ld(0)}},ld=iE("applyAff","Effect.Aff",function(){return{apply:$u(fd),Functor0:function(){return Si}}}),cE=ld(71);var xe={liftEffect:Qh,Monad0:function(){return fd}},fE=function(){var t=le(xe);return function(r){return LI(E(t(r)))}}(),lE=function(t){return rl(function(r){return p(R)(fE)(t.join(r))})};var _E=function(t){return function(r){return ct(Fe)(le(xe)(r.isSuspended))(function(e){return e?le(xe)(tr(R)(r.kill(t,E(f(ft)(void 0))))):rl(function(n){return p(R)(fE)(r.kill(t,n))})})}};var Fn={parallel:ot,sequential:rE,Monad0:function(){return fd},Applicative1:function(){return WI(0)}},WI=iE("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=xn(Fn),r=f(da);return function(e){return t(r(e))}}(),Apply0:function(){return xi}}});var qI={append:function(t){return function(r){return function(e){return nE(Fn)(Lt)([t(e),r(e)])}}}};var HI=E(f(da)(void 0)),pE={mempty:HI,Semigroup0:function(){return qI}};var sE={alt:Zh,Functor0:function(){return nl}};var vE=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),V_=function(){function t(){}return t.value=new t,t}(),Kc=function(){function t(){}return t.value=new t,t}(),G_=function(){function t(){}return t.value=new t,t}(),Yc=function(){function t(){}return t.value=new t,t}(),J_=function(){function t(){}return t.value=new t,t}(),j_=function(){function t(){}return t.value=new t,t}(),mE=function(){function t(){}return t.value=new t,t}(),ev=function(){function t(){}return t.value=new t,t}(),nv=function(){function t(){}return t.value=new t,t}(),X_=function(){function t(){}return t.value=new t,t}(),Q_=function(){function t(){}return t.value=new t,t}(),DE=function(){function t(){}return t.value=new t,t}(),al=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),_d=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}();var zI="numeric",VI="sudden",GI="unit",JI="cancel",jI="step",XI="linear",QI="exponential",KI="envelope",dE=function(t,r,e,n){if(e.type===VI)t.value=e.value.n;else if(e.type===GI)r.id&&ZI(r.id,n),n.units[e.value.i].main.connect(t),r.id=e.value.i;else if(e.type===zI)t[e.value.t.type===jI?"setValueAtTime":e.value.t.type===XI?"linearRampToValueAtTime":e.value.t.type===QI?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](e.value.n,e.value.o);else if(e.type===JI)e.value.hold?t.cancelAndHoldAtTime(e.value.o):t.cancelScheduledValues(e.value.o);else if(e.type===KI){let a=e.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(e.value.p,a,e.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(e))},YI=function(t,r,e,n,a){return n[e]||(n[e]={}),dE(r.parameters.get(e),n[e],a,t)},zu=function(t,r,e,n,a){return n[e]||(n[e]={}),dE(r[e],n[e],a,t)},ye=function(t,r,e,n){let a=t("@fan@")(u=>u)(e);n.scopes[a]||(n.scopes[a]=[]),n.scopes[a].push(r),n.units[r].scope=a},Ae=function(t,r){r.toConnect[t]&&(r.toConnect[t].forEach(function(e){e.w?r.units[e.w]?e.f():(r.toConnect[e.w]||(r.toConnect[e.w]=[]),r.toConnect[e.w].push({f:e.f})):e.f()}),delete r.toConnect[t])},ke=function(t,r,e,n){t()(a=>bE(r,a,n))(e)},bE=function(t,r,e){var n=function(){e.units[t].audioOutgoing.push(r),e.units[t].pendingOn||(e.units[t].main.connect(e.units[r].main),e.units[r].se&&e.units[t].main.connect(e.units[r].se))};if(!e.units[t]){e.toConnect[t]||(e.toConnect[t]=[]);var a={f:n};r!==t&&!e.units[r]&&(a.w=r),e.toConnect[t].push(a);return}if(!e.units[r]){e.toConnect[r]||(e.toConnect[r]=[]);var a={f:n};r!==t&&!e.units[t]&&(a.w=t),e.toConnect[r].push(a);return}n()};function pd(t){return function(r){return function(){delete r.units[t.id]}}}function sd(t){return function(r){return function(){bE(t.from,t.to,r)}}}var ZI=function(t,r){if(r.units[t].scope==="@fan@")return;let e=r.units[t].scope;r.scopes[e].forEach(n=>{delete r.units[n]}),delete r.scopes[e]};function vd(t){return function(r){return function(){var e=t.from,n=t.to;if(!r.units[e]||(r.units[e].audioOutgoing=r.units[e].audioOutgoing.filter(function(u){return u!==n}),r.units[e].main.disconnect(r.units[n].main),r.units[n].se&&r.units[e].main.disconnect(r.units[n].se),r.units[e].scope==="@fan@"))return;let a=r.units[e].scope;r.scopes[a].forEach(u=>{delete r.units[u]}),delete r.scopes[a]}}}var md=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:r.q,frequency:r.frequency})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Dd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=new AnalyserNode(e.context,r),o=a(u)();e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:a,analyser:o,main:e.context.createGain(),se:u},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},dd=t=>r=>e=>()=>{var n=r.id,a=r.options;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,a.name,{numberOfInputs:a.numberOfInputs,numberOfOutputs:a.numberOfOutputs,outputChannelCount:a.outputChannelCount,parameterData:a.parameterData,processorOptions:a.processorOptions})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},bd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:r.q,frequency:r.frequency})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},yd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new ConstantSourceNode(o,i)},u={offset:r.offset};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Ad=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:r.buffer})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},kd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:r.delayTime,maxDelayTime:r.maxDelayTime})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},gd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:r.knee,ratio:r.ratio,threshold:r.threshold,attack:r.attack,release:r.release})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Cd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:r.gain})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},hd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:r.q,frequency:r.frequency})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Ed=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:r.frequency,gain:r.gain})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Td=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:r.feedforward,feedback:r.feedback})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Sd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new AudioBufferSourceNode(o,i)},u={loop:!0,buffer:r.buffer,loopStart:r.loopStart,loopEnd:r.loopEnd,playbackRate:r.playbackRate};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},xd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:r.q,frequency:r.frequency})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Fd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:r.frequency,gain:r.gain})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Od=t=>r=>e=>()=>{var n=r.id,a=r.element,u=function(){var o=e.context.createMediaElementSource(a);return o};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},$d=t=>r=>e=>()=>{var n=r.id;e.units[r.id]={main:e.context.createMediaStreamSource(r.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},wd=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:r.frequency,Q:r.q})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Md=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:r.frequency,Q:r.q,gain:r.gain})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Pd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){var _={frequency:i.frequency,periodicWave:i.spec.type==="wave"?i.spec.value:bb(e.context)(i.spec.value.real)(i.spec.value.img)()},m=new OscillatorNode(o,_);return m},u={frequency:r.frequency,type:"custom",spec:r.spec};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Id=t=>r=>e=>()=>{var n=r.id,a=function(o,i){var _={loop:i.loop,buffer:i.buffer,playbackRate:i.playbackRate};return new AudioBufferSourceNode(o,_)},u={loop:!1,buffer:r.buffer,playbackRate:r.playbackRate,bufferOffset:r.bufferOffset,duration:t(void 0)(o=>o)(r.duration)};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Rd=t=>r=>e=>()=>{var n=r.id,a=r.cb,u=e.context.createMediaStreamDestination(),o=new MediaRecorder(u.stream);a(o)(),o.start(),e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:a,recorder:o,main:e.context.createGain(),se:u},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Nd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"sawtooth"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Ld=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"sine"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Bd=t=>r=>()=>{r.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:r.context.createGain(),se:r.context.destination}},Ud=t=>r=>e=>()=>{var n=r.id;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:r.pan})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Wd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"square"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},qd=t=>r=>e=>()=>{var n=r.id,a=function(o,i){return new OscillatorNode(o,i)},u={frequency:r.frequency,type:"triangle"};e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:a,onOff:!1,pendingOn:!0,main:a(e.context,u)},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)},Hd=t=>r=>e=>()=>{var n=r.id,a=r.curve,u=r.oversample;e.units[n]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:a,oversample:u.type})},ye(t,n,r.scope,e),Ae(n,e),ke(t,n,r.parent,e)};function zd(t){return function(r){return function(){var e=t.id,n=t.cb;r.units[e].analyserOrig!==n&&(r.units[e].analyser&&r.units[e].analyser(),r.units[e].analyser=n(r.units[e].se)(),r.units[e].analyserOrig=n)}}}function Vd(t){return function(r){return function(){var e=t.cb,n=t.id;if(r.units[n].recorderOrig!==e){r.units[n].recorder&&r.units[n].recorder.stop();var a=e;r.units[n].recorderOrig=e;var u=new MediaRecorder(r.units[n].se);a(u)(),u.start()}}}}function Gd(t){return function(r){return function(){var e=t.id,n=t.curve;r.units[e].main.curve=n}}}function Jd(t){return function(r){return function(){var e=t.id,n=t.paramName,a=t.paramValue;YI(r,r.units[e].main,n,r.units[e].controllers,a)}}}var Vu=function(t,r,e){r.resume&&t.value.n!==void 0&&(r.resume[e]=t.value.n)};function jd(t){return function(r){return function(){var e=t.id,n=t.gain;zu(r,r.units[e].main,"gain",r.units[e].controllers,n),Vu(n,r.units[e],"gain")}}}function Xd(t){return function(r){return function(){var e=t.id,n=t.q;zu(r,r.units[e].main,"Q",r.units[e].controllers,n),Vu(n,r.units[e],"Q")}}}function Qd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].resume&&(r.units[e].resume.buffer=n)}}}function Kd(t){return function(r){return function(){var e=t.id,n=t.buffer;r.units[e].main.buffer=n}}}function Yd(t){return function(r){return function(){var e=t.id,n=t.spec;r.units[e].resume&&(r.units[e].resume.spec=n)}}}function Zd(t){return function(r){return function(){var e=t.id,n=t.pan;zu(r,r.units[e].main,"pan",r.units[e].controllers,n),Vu(n,r.units[e],"pan")}}}function tb(t){return function(r){return function(){var e=t.id,n=t.threshold;zu(r,r.units[e].main,"threshold",r.units[e].controllers,n),Vu(n,r.units[e],"threshold")}}}function rb(t){return function(r){return function(){var e=t.id,n=t.loopStart;r.units[e].main.loopStart=n,r.units[e].resume.loopStart=n}}}function eb(t){return function(r){return function(){var e=t.id,n=t.loopEnd;r.units[e].main.loopEnd=n,r.units[e].resume.loopEnd=n}}}function nb(t){return function(r){return function(){var e=t.id,n=t.bufferOffset;r.units[e].resume.bufferOffset=n}}}function ab(t){return function(r){return function(e){return function(){var n=r.id,a=r.duration;e.units[n].duration=t(void 0)(u=>u)(a)}}}}function ub(t){return function(r){return function(){var e=t.id,n=t.release;zu(r,r.units[e].main,"release",r.units[e].controllers,n),Vu(n,r.units[e],"release")}}}function ob(t){return function(r){return function(){var e=t.id,n=t.offset;zu(r,r.units[e].main,"offset",r.units[e].controllers,n),Vu(n,r.units[e],"offset")}}}function ib(t){return function(r){return function(){var e=t.id,n=t.ratio;zu(r,r.units[e].main,"ratio",r.units[e].controllers,n),Vu(n,r.units[e],"ratio")}}}function cb(t){return function(r){return function(){var e=t.id,n=t.attack;zu(r,r.units[e].main,"attack",r.units[e].controllers,n),Vu(n,r.units[e],"attack")}}}function fb(t){return function(r){return function(){var e=t.id,n=t.knee;zu(r,r.units[e].main,"knee",r.units[e].controllers,n),Vu(n,r.units[e],"knee")}}}function lb(t){return function(r){return function(){var e=t.id,n=t.delayTime;zu(r,r.units[e].main,"delayTime",r.units[e].controllers,n),Vu(n,r.units[e],"delayTime")}}}function _b(t){return function(r){return function(){var e=t.id,n=t.playbackRate;zu(r,r.units[e].main,"playbackRate",r.units[e].controllers,n),Vu(n,r.units[e],"playbackRate")}}}function pb(t){return function(r){return function(){var e=t.id,n=t.frequency;zu(r,r.units[e].main,"frequency",r.units[e].controllers,n),Vu(n,r.units[e],"frequency")}}}function sb(t){return function(r){return function(){var e=t.id,n=t.onOff;n.x.type==="on"?tR(e)(n)(r)():n.x.type==="off"&&rR(e)(n)(r)()}}}var tR=function(t){return function(r){return function(e){return function(){if(!e.units[t].onOff){e.units[t].pendingOn=!1,e.units[t].onOff=!0,e.units[t].main=e.units[t].createClosure(e.context,e.units[t].resume);for(var n=0;n<e.units[t].audioOutgoing.length;n++){var a=e.units[t].audioOutgoing[n];e.units[t].main.connect(e.units[a].main),e.units[a].se&&e.units[t].main.connect(e.units[a].se)}e.units[t].resume&&e.units[t].resume.bufferOffset?typeof e.units[t].resume.duration=="number"?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset,e.units[t].resume.duration):e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.bufferOffset):e.units[t].resume&&e.units[t].resume.loopStart?e.units[t].main.start(e.deprecatedWriteHead+r.o,e.units[t].resume.loopStart):e.units[t].main.start(e.deprecatedWriteHead+r.o)}}}}},rR=function(t){return function(r){return function(e){return function(){if(!!e.units[t].onOff){e.units[t].onOff=!1;var n=e.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(e.deprecatedWriteHead+r.o)}}}}};function vb(t){for(var r=new Float32Array(t.length),e=0;e<t.length;e++)r[e]=t[e];return r}function av(t){return function(){t.stop()}}function mb(t){return function(r){return function(e){return function(){var n=[];e.ondataavailable=function(a){n.push(a.data)},e.onstop=function(){var a=new Blob(n,{type:t});r(a)(),n=null}}}}}function Db(t){return function(r){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:r})}}}function K_(t){return function(){var r=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(r),r}}function db(t){return function(){var r=t.createConstantSource();return r.offset.value=0,r.connect(t.destination),r.start(),function(){r.stop(),r.disconnect(t.destination)}}}var bb=function(t){return function(r){return function(e){return function(){for(var n=new Float32Array(r.length),a=new Float32Array(e.length),u=0;u<r.length;u++)n[u]=r[u];for(var u=0;u<e.length;u++)a[u]=e[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function fc(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function yb(t){return function(){t.close()}}function Ab(t){return function(){return fetch(t).then(function(r){return r.arrayBuffer()},function(r){return console.error("Error fetching buffer",r),Promise.reject(r)})}}function kb(t){return function(r){return function(){return t.decodeAudioData(r)}}}function gb(){return new(window.AudioContext||window.webkitAudioContext)}function Cb(t){return function(){return t.state}}function Y_(t){return function(){return t.currentTime}}function yE(t){return function(r){return function(e){return function(){t.then(e,r)}}}}var aR=function(t){return function(r){return rl(function(e){return fp(R)(vr(pE))(yE(r)(function(n){return e(Qt.create(t(n)))()})(function(n){return e(Kt.create(n))()}))})}};var uR=function(t){return La(function(r){return Wo("Promise failed, couldn't extract JS Error or String")})(j(K))(xD(O(SD(vm)(Mu))(MD(Mu)("Error")(t))(p(F_(Qu))(Wo)(PD(Mu)(t)))))},AE=aR(uR),uv=function(t){return ct(Fe)(le(xe)(t))(AE)};function hb(t){return function(){return URL.createObjectURL(t)}}var kE=function(t){return function(r){return function(e){return Tt(mb(t))(e)(function(){var n=Jn(Yn)(r);return function(a){return n(hb(a))}}())}}};var Zc=function(t){return{ids:function(){var e=Qe(t)(),n=Wt(Na)(ic(Wc(ds))({newSeed:nc(e),size:5}));return tr(rr)(tu(Sr(Qa)(1))(t))(),n},deleteFromCache:pd,disconnectXFromY:vd,connectXToY:sd,makeAllpass:md(Nt),makeAnalyser:Dd(Nt),makeAudioWorkletNode:dd(Nt),makeBandpass:bd(Nt),makeConstant:yd(Nt),makeConvolver:Ad(Nt),makeDelay:kd(Nt),makeDynamicsCompressor:gd(Nt),makeGain:Cd(Nt),makeHighpass:hd(Nt),makeHighshelf:Ed(Nt),makeIIRFilter:Td(Nt),makeLoopBuf:Sd(Nt),makeLowpass:xd(Nt),makeLowshelf:Fd(Nt),makeMediaElement:Od(Nt),makeMicrophone:$d(Nt),makeNotch:wd(Nt),makePeaking:Md(Nt),makePeriodicOsc:Pd(Nt),makePlayBuf:Id(Nt),makeRecorder:Rd(Nt),makeSawtoothOsc:Nd(Nt),makeSinOsc:Ld(Nt),makeSpeaker:Bd,makeSquareOsc:Wd(Nt),makeStereoPanner:Ud(Nt),makeTriangleOsc:qd(Nt),makeWaveShaper:Hd(Nt),setAnalyserNodeCb:zd,setMediaRecorderCb:Vd,setWaveShaperCurve:Gd,setAudioWorkletParameter:Jd,setBuffer:Qd,setConvolverBuffer:Kd,setDuration:ab(Nt),setPeriodicOsc:Yd,setOnOff:sb,setBufferOffset:nb,setLoopStart:rb,setLoopEnd:eb,setRatio:ib,setOffset:ob,setAttack:cb,setGain:jd,setQ:Xd,setPan:Zd,setThreshold:tb,setRelease:ub,setKnee:fb,setDelay:lb,setPlaybackRate:_b,setFrequency:pb}},At=function(t){return function(r){return ct(Fe)(uv(Ab(r)))(function(){var e=kb(t);return function(n){return uv(e(n))}}())}},Z_=function(t){var r=le(t);return function(e){return r(Cb(e))}};var na=function(t){return le(t)(gb)},Gu=function(t){var r=le(t);return function(e){return r(db(e))}},bn=function(t){return function(r){return le(t)(function(){var n=Z_(te)(r)();return On(ft)(n!=="closed")(yb(r))()})}},_R=ot,pR=ot,ov=function(t){return function(r){return p(Si)(function(e){return{microphone:function(){return t?f(Eo)(_R(e)):L.value}(),camera:function(){return r?f(Eo)(pR(e)):L.value}()}})(uv(Db(t)(r)))}};var jo=function(){function t(){}return t.value=new t,t}(),Xo=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),xu=function(){function t(){}return t.value=new t,t}(),an=cd,Fi=function(t){return bo(Fn)(O(sE)(xn(Fn)(ct(Fe)(lE(t))(le(xe))))(xn(Fn)(_E(Wo("We navigated away from the page"))(t))))},ul=function(t){return function(r){return function(e){return function(n){return O(t)(f(r)(xu.value))(n)}}}},wa=function(t){return function(r){return function(e){return function(n){return O(t)(f(r)(J(fe)(ue.value)(Gr(E(n)))))(p(t.Functor0())(function(a){return J(fe)(ue.value)(Gr(E(X(st)(a)(n))))})(p(t.Functor0())(function(a){return a.value0})(e)))}}}},iv=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return p(k)(function(o){return J(fe)(ue.value)(Gr(E(function(){if(o.value0 instanceof jo)return f(ft)(void 0);if(o.value0 instanceof Xo)return X(st)(X(st)(o.value0.value0)(t(f(ft)(void 0))))(r(xu.value));if(o.value0 instanceof xu)return function(){o.value1(),r(jo.value)();var _=yo(ct(Fe)(na(xe))(function(m){return ct(Fe)(Gu(xe)(m))(function(l){return ct(Fe)(e(m))(function(s){return le(xe)(function(){var c=n(m)(s)(),d=X(st)(X(st)(c)(l))(bn(te)(m));return r(new Xo(d))(),d})})})}))();return t(function(){return r(xu.value)(),Jo(Fi(_))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 58, column 21 - line 76, column 26): "+[o.value0.constructor.name])}())))})(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(o){return o.value0})(a)))(p(k)(Q.create)(u)))}}}}}},Ma=function(t){return function(r){return function(e){return function(){return t(e)(),r(new vE(e))()}}}},cv=function(t){return function(r){return function(e){return function(n){return function(a){return Ve(function(u){return function(o){var i=ul($)(g)(r)(o);return Ff(O($)(f(g)(J(ts)(Bt.value)("cursor: pointer;")))(iv(e)(u)(n)(a)(r)(i)))([tn(p(k)(function(_){if(_ instanceof xu)return t;if(_ instanceof jo)return"\u23F3";if(_ instanceof Xo)return"\u{1F6D1}";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 126, column 17 - line 129, column 35): "+[_.constructor.name])})(i))])}})}}}}},ht=function(t){return function(r){return function(e){return function(n){return Ve(function(a){return function(u){var o=ul($)(g)(t)(u);return dn(iv(r)(a)(e)(n)(t)(o))([tn(p(k)(function(i){if(i instanceof xu)return"Turn on";if(i instanceof jo)return"Loading...";if(i instanceof Xo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.Util (line 100, column 17 - line 103, column 42): "+[i.constructor.name])})(o))])}})}}}};var ol=function(t){return function(r){return function(){var n=fc(t)(),a=hn(Cn)(Oe(0))(),u=Re(Xc([new mi(p(k)(function(o){return Ef.create(fk(o))})(r))])(Zc(a)))(function(o){return o(n)})();return u}}};var pt=function(t){return function(r){return function(){var n=fc(t)(),a=hn(Cn)(Oe(0))(),u=Re(Xc(r)(Zc(a)))(function(o){return o(n)})();return u}}},fv=function(t){return function(){var e=na(te)();return p(R)(function(n){return X(st)(n)(bn(te)(e))})(pt(e)(t))()}};var sR=function(){return D.value}(),gE=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.2
      [ b
      , allpass_ 700.0
          [ allpass_ { frequency: 990.0, q: 20.0 } [ b ]
          , allpass_ 1110.0
              [ b
              , allpass_ { frequency: 2010.0, q: 30.0 } [ b ]
              ]
          ]
      ]
  ]
</code></pre>

  @allpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(D.value)(sR)({allpass:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Oa(ar(br)(a)(it()))(function(u){return function(o){return Ft(nt)(.2)([u,W_(ad)(700)([W_(Xs(bt(dt()(q(q(Dt)(VD)()()()({reflectSymbol:function(){return"q"}}))(Js)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:990,q:20})([u]),W_(ad)(1110)([u,W_(Xs(bt(dt()(q(q(Dt)(VD)()()()({reflectSymbol:function(){return"q"}}))(Js)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:2010,q:30})([u])])])])}})])}}))})}}};function Oi(t){return function(e,n,a){if(n===null)return new t(e);var u=e.byteLength,o=t.BYTES_PER_ELEMENT,i=Math.min(u,n>>>0);if(a===null)return new t(e,i);var _=Math.min((u-i)/o,a);return new t(e,i,_)}}var mR=Oi(Uint8ClampedArray),DR=Oi(Uint32Array),dR=Oi(Uint16Array),CE=Oi(Uint8Array),bR=Oi(Int32Array),yR=Oi(Int16Array),AR=Oi(Int8Array),kR=Oi(Float32Array),gR=Oi(Float64Array);function hE(t){for(var r=t.length,e=new Array(r),n=0;n<r;n++)e[n]=t[n];return e}var lv={create:CE,BinaryValue0:function(){}};var _v=function(t){return function(r){return function(){return hE(r)}}};var il=gu,cl=gu,fl=gu,uu=gu,ou=gu,iu=gu,cu=gu,fu=gu;function pv(t){return t|0}var xR=function(t,r,e){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+r+", line "+u+")",r,u);return n=1,a=e(),n=2,a}},lc=Sa(function(t){return function(){var e=Ai(),n=Nr(!0)(),a=xR("fx","FRP.Event.Animate",function(){return tr(R)(Tt(gs)(e)(function(){var i=Vr(n)();return On(ft)(i)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),se(!1)(n)}});var FR="background-color: rgb(150,30,10);",OR="background-color: rgb(130,60,10);",$R="background-color: rgb(80,90,10);",wR="background-color: rgb(10,130,10);",MR="background-color: rgb(10,100,0);",PR=Ms(eu)(function(t){return xr(Tr($s)()(za)()(S_))(FR)(xr(Tr(ea)()(_n)()(za))(OR)(xr(Tr(Cu)()(pn)()(_n))($R)(xr(Tr(hu)()(sn)()(pn))(wR)(xr(Tr(Eu)()(Tu)()(sn))(MR)(Hu)))))}),IR=function(t){return function(r){return function(e){return function(n){return Zs(Ys(bt(dt()(q(q(Dt)(Ks)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(mt()())))({cb:n,fftSize:Ws.value})([ar(r)(e)(it())])}}}},RR=function(){return D.value}(),$r="background-color: rgb(255,255,255,0.0);",Mr=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(_){return function(m){return function(l){return p(t)(function(s){var v=x_(r)()(x_(n)()(s)(_))(m);return v?J(u)(Bt.value)(x_(r)()(x_(n)()(PR)(_))(m)):J(u)(Bt.value)($r)})(l)}}}}}}}}}}},NR=function(){return 15/40}(),LR=function(){return 10/40}(),BR=function(){return 7/40}(),UR=function(){return 3/40}(),WR=function(){return 1/40}(),xE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(P()(G)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))(RR)({analyser:M(Ve(function(n){return function(a){var u=Gl(bu)(j(K))(a),o=ul($)(g)(e)(function(_){return _.right}(u)),i=function(_){return _.left}(u);return Lr([dn(O($)(f(g)(J(Sf)(Bt.value)("cursor: pointer;")))(iv(t)(function(_){return n(Kt.create(_))})(function(_){return At(_)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(_){return function(m){return function(){var s=Nr(L.value)(),v=fc(_)(),c=hn(Cn)(Oe(0))(),d=Xc([IR(Qs)(br)(m)(function(Z){return function(){return se(new T(Z))(s)(),se(L.value)(s)}})])(Zc(c)),rt=Re(O($)(p(k)(Kt.create)(d))(p(k)(Qt.create)(lc)))(function(Z){if(Z instanceof Kt)return Z.value0(v);if(Z instanceof Qt)return function(){var zt=Vr(s)();return kn(ft)(jr)(zt)(function(Jr){return function(){var ut=K_(Jr)(),It=_v(lv)(ut)(),Cr=Nr(0)(),Et=Nr(0)(),ee=Nr(0)(),Hn=Nr(0)(),Be=Nr(0)(),ya=Nr(0)(),ua=Nr(0)(),Ue=Nr(0)(),Ju=Nr(0)(),Yo=Nr(0)(),ef=function(zn){if(zn<32)return Cr;if(zn<64)return Et;if(zn<96)return ee;if(zn<128)return Hn;if(zn<168)return Be;if(zn<160)return ya;if(zn<224)return ua;if(Qr)return Ue;throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 146, column 45 - line 154, column 63): "+[zn.constructor.name])};Pu(It)(function(zn){var bc=pv(zn);return function(){var Zo=Vr(Yo)();return ai(Sr(Qa)(bc))(Ju)(),ai(Sr(Qa)(bc))(ef(Zo))(),ai(Sr(Qa)(1))(Yo)()}})();var Ia=Mn(fC)(ft)(function(zn){return function(){var cp=p(R)(Ur)(Vr(zn))(),Zo=p(R)(Yu(Fl)(cp))(p(R)(Ur)(Vr(Ju)))();return xr(Tr($s)()(za)()(S_))(Zo>NR)(xr(Tr(ea)()(_n)()(za))(Zo>LR)(xr(Tr(Cu)()(pn)()(_n))(Zo>BR)(xr(Tr(hu)()(sn)()(pn))(Zo>UR)(xr(Tr(Eu)()(Tu)()(sn))(Zo>WR)(Hu)))))}})(xr(Tr(uC)()(kD)()(cC))(Cr)(xr(Tr(oC)()(gD)()(kD))(Et)(xr(Tr(iC)()(S_)()(gD))(ee)(xr(Tr($s)()(za)()(S_))(Hn)(xr(Tr(ea)()(_n)()(za))(Be)(xr(Tr(Cu)()(pn)()(_n))(ya)(xr(Tr(hu)()(sn)()(pn))(ua)(xr(Tr(Eu)()(Tu)()(sn))(Ue)(Hu)))))))))();return n(new Qt(Ia))()}})()};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 127, column 57 - line 165, column 57): "+[Z.constructor.name])})();return function(){return rt(),function(){var zt=Z_(te)(_)();return On(ft)(zt!=="closed")(bn(te)(_))()}(),n(new Qt(Ms(eu)(E(Ms(Fa)(E(!1))))))()}}}})(e)(o)))([tn(p(k)(function(_){if(_ instanceof xu)return"Turn on";if(_ instanceof jo)return"Loading...";if(_ instanceof Xo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Analyser (line 178, column 31 - line 181, column 56): "+[_.constructor.name])})(o))]),dr(f(g)(J(lt)(Bt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(Ce)(mo)(lt)(Da)(mo)(fu)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(Bn)(vo)(lt)(Da)(vo)(cu)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(Ln)(so)(lt)(Da)(so)(iu)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(Nn)(po)(lt)(Da)(po)(ou)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(Rn)(_o)(lt)(Da)(_o)(uu)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(Fa)(lo)(lt)(Da)(lo)(fl)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(zo)(fo)(lt)(Da)(fo)(cl)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ce)(Da)(Ho)(co)(lt)(Da)(co)(il)(fu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(Ce)(mo)(lt)(ma)(mo)(fu)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(Bn)(vo)(lt)(ma)(vo)(cu)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(Ln)(so)(lt)(ma)(so)(iu)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(Nn)(po)(lt)(ma)(po)(ou)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(Rn)(_o)(lt)(ma)(_o)(uu)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(Fa)(lo)(lt)(ma)(lo)(fl)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(zo)(fo)(lt)(ma)(fo)(cl)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Bn)(ma)(Ho)(co)(lt)(ma)(co)(il)(cu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(Ce)(mo)(lt)(va)(mo)(fu)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(Bn)(vo)(lt)(va)(vo)(cu)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(Ln)(so)(lt)(va)(so)(iu)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(Nn)(po)(lt)(va)(po)(ou)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(Rn)(_o)(lt)(va)(_o)(uu)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(Fa)(lo)(lt)(va)(lo)(fl)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(zo)(fo)(lt)(va)(fo)(cl)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Ln)(va)(Ho)(co)(lt)(va)(co)(il)(iu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(Ce)(mo)(lt)(sa)(mo)(fu)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(Bn)(vo)(lt)(sa)(vo)(cu)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(Ln)(so)(lt)(sa)(so)(iu)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(Nn)(po)(lt)(sa)(po)(ou)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(Rn)(_o)(lt)(sa)(_o)(uu)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(Fa)(lo)(lt)(sa)(lo)(fl)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(zo)(fo)(lt)(sa)(fo)(cl)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Nn)(sa)(Ho)(co)(lt)(sa)(co)(il)(ou)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(Ce)(mo)(lt)(pa)(mo)(fu)(uu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(Bn)(vo)(lt)(pa)(vo)(cu)(uu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(Ln)(so)(lt)(pa)(so)(iu)(uu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(Nn)(po)(lt)(pa)(po)(ou)(uu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(Rn)(_o)(lt)(pa)(_o)(uu)(uu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(Fa)(lo)(lt)(pa)(lo)(fl)(uu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(zo)(fo)(lt)(pa)(fo)(cl)(uu)(i)))([]),dr(O($)(f(g)(J(lt)(Bt.value)($r)))(Mr(k)(Rn)(pa)(Ho)(co)(lt)(pa)(co)(il)(uu)(i)))([])])])}}))})}}};var HR=function(){return D.value}(),FE=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.8
      [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
      , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
      , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
      , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
      , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
      ]
  ]</code></pre>

  @bandpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))(D.value)(HR)({bandpass:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Oa(ar(br)(a)(it()))(function(u){return function(o){return Ft(nt)(.8)([Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:400,q:1})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:880,q:5})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:1200,q:10})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:2e3,q:20})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var VR=function(){return D.value}(),OE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="compression">Compression</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">Compression</a>, when used judiciously, can make certain sounds sit better in a mix, like for example vocals. The <a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">MDN Web Audio documentation</a> does an excellent job explaining how its parameters work. When used not-judiciously, it makes everything sound loud, and who likes that? So let's use it judiciously, like in the example below. We'll pass an object that only specifies the threshold and otherwise use the default options for the compressor.</p>

  <pre><code>
-- defaultDynamicsCompressor =
--   { ratio: 12.0
--   , attack: 0.003
--   , release: 0.25
--   , knee: 30.0
--   , threshold: -24.0
--   }
run2_
  [ dynamicsCompressor_ { threshold: -50.0 }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~compression~
  </section>
`}})()()(P()(G)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))(VR)({compression:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Wh(Ph(bt(dt()(Dt))(mt()())))({})([ar(br)(a)(it())])])}}))})}}};var aa=function(){return function(t){var r=Ye(),e=oe()({reflectSymbol:function(){return"playbackRate"}})(D.value),n=Gc(t);return function(a){return r(e(n(a)))}}},tf=function(){return function(t){var r=Ye(),e=oe()({reflectSymbol:function(){return"onOff"}})(D.value),n=HC(t);return function(a){return r(e(n(a)))}}},$E=function(){return function(t){var r=Ye(),e=oe()({reflectSymbol:function(){return"offset"}})(D.value),n=Gc(t);return function(a){return r(e(n(a)))}}},wE=function(){var t=Ye(),r=oe()({reflectSymbol:function(){return"loopStart"}})(D.value);return function(e){return t(r(e))}},ME=function(){var t=Ye(),r=oe()({reflectSymbol:function(){return"loopEnd"}})(D.value);return function(e){return t(r(e))}},yn=function(){return function(t){var r=Ye(),e=oe()({reflectSymbol:function(){return"gain"}})(D.value),n=Gc(t);return function(a){return r(e(n(a)))}}},Ao=function(){return function(t){var r=Ye(),e=oe()({reflectSymbol:function(){return"frequency"}})(D.value),n=Gc(t);return function(a){return r(e(n(a)))}}};var ll=function(){return function(t){var r=Ye(),e=oe()({reflectSymbol:function(){return"delayTime"}})(D.value),n=Gc(t);return function(a){return r(e(n(a)))}}};var JR=function(){return D.value}(),PE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(P()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))(JR)({tf:M(Zr("<|>")),txt:M(Zr(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( pure $ offset $ AudioEnvelope
                  { d: 5.0
                  , o: 0.1
                  , p: 0 .. 1920 # mapWithIndex
                      \\i -> const $
                      if i \`mod\` 3 == 0 then 1.0
                      else 0.0
                  }
              )
          )
      ]
  ]`)),constant:M(ht(e)(t)(function(n){return f(da)(void 0)})(function(n){return function(a){return pt(n)([Ft(nt)(.5)([rv(Gs)(0)(O($)(it())(f(g)($E()(Sn)({d:5,o:.1,p:Fo(ii)(function(u){return E(function(){var o=Ya(Ku)(u)(3)===0;return o?1:0}())})(un(0)(1920))}))))])])}}))})}}};var XR=function(){return D.value}(),IE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(P()(G)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))(XR)({convolution:M(ht(e)(t)(function(n){return $t(cE)(p(Si)(function(a){return function(u){return{loop:a,verb:u}}})(At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(At(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return pt(n)([Ih(nh)(a.verb)([ar(br)(a.loop)(it())])])}}))})}}};var KR=function(){return D.value}(),RE=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (playBuf buf bangOn)
      \\b _ -> gain_ 0.2
        [ delay_ 0.03 [ b ]
        , delay_ 0.1 [ b ]
        , delay_ 0.3 [ b ]
        , delay_ 0.7 [ b ]
        ]
  ]</code></pre>

  @delay@
  </section>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))(D.value)(KR)({delay:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return pt(n)([Oa(qn(Va)(a)(it()))(function(u){return function(o){return Ft(nt)(.2)([Do(Je)(.03)([u]),Do(Je)(.1)([u]),Do(Je)(.3)([u]),Do(Je)(.7)([u])])}})])}}))})}}};var ZR=function(){return D.value}(),NE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="gain">Gain</h2>
  <p>The almighty <a href="https://developer.mozilla.org/en-US/docs/Web/API/GainNode">gain</a> node is your friendly neighborhood volume control. Volume in the web-audio API goes from 0 to 1 whereas we hear logarithmically, so when you're using this, make sure to convert between decibels and gain if you want to work with more intuitive units. The conversion formula is as follows:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>And here's a little example of a single gain node:</p>

  <pre><code>run2_
  [ gain_ 0.1
    [ loopBuf buf bangOn ]
  ] </code></pre>

  ~gain~
  </section>
`}})()()(P()(G)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))(ZR)({gain:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return pt(n)([Ft(nt)(.1)([ar(br)(a)(it())])])}}))})}}};var rN=function(){return D.value}(),LE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(P()(G)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))(rN)({highpass:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Kf(nu)(2e3)([ar(br)(a)(it())])])}}))})}}};var nN=function(){return D.value}(),BE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(P()(G)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))(nN)({highshelf:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([qh(Mh(bt(dt()(q(q(Dt)(lh)()()()({reflectSymbol:function(){return"gain"}}))(_h)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:2e3,gain:.4})([ar(br)(a)(it())])])}}))})}}};var uN=function(){return D.value}(),UE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Ocarina enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 : 0.0004059599 : 0.00020298 : empty)
          /\\ (1.0126964558 : -1.9991880801 : 0.9873035442 : empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(P()(G)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}}))(uN)({iirFilterEx:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Rh()()(eh(ki)(ki))(new Q(Lc()()(20298e-8)(Lc()()(.0004059599)(Lc()()(20298e-8)(Pm))),Lc()()(1.0126964558)(Lc()()(-1.9991880801)(Lc()()(.9873035442)(Pm)))))([ar(br)(a)(it())])])}}))})}}};var iN=function(){return D.value}(),WE=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2 id="loopbuf">Looping buffer</h2>

  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">looping buffer</a> is buffered audio that loops. The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start and end and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [ loopBuf
      { buffer: buf
      , playbackRate: 0.5
      , loopStart: 0.1
      , loopEnd: 0.6
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.0
      , loopStart: 0.5
      , loopEnd: 1.2
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.7
      }
      bangOn
  ]</code></pre>

  @loopBuf@
  </section>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))(D.value)(iN)({loopBuf:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return pt(n)([ar(jc(bt(dt()(q(q(q(q(Dt)(jf)()()()({reflectSymbol:function(){return"playbackRate"}}))(N_)()()()({reflectSymbol:function(){return"loopStart"}}))(R_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Jc)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(it()),ar(jc(bt(dt()(q(q(q(q(Dt)(jf)()()()({reflectSymbol:function(){return"playbackRate"}}))(N_)()()()({reflectSymbol:function(){return"loopStart"}}))(R_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Jc)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(it()),ar(jc(bt(dt()(q(q(Dt)(jf)()()()({reflectSymbol:function(){return"playbackRate"}}))(Jc)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())))({buffer:a,playbackRate:1.7})(it())])}}))})}}};var fN=function(){return D.value}(),qE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(P()(G)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))(fN)({lowpass:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Yf(js)(215)([ar(br)(a)(it())])])}}))})}}};var _N=function(){return D.value}(),HE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(P()(G)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))(_N)({lowshelf:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([zh(wh(bt(dt()(q(q(Dt)(ih)()()()({reflectSymbol:function(){return"gain"}}))(ch)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:91,gain:.4})([ar(br)(a)(it())])])}}))})}}};var sN=function(){return D.value}(),zE=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so.</p>

  <blockquote>Make sure to use \u{1F3A7} when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

  <pre><code>\\mic -> run2_
  [ case mic of
      Just m -> fix \\i -> gain_ 1.0
        [ microphone m
        , delay_ 0.1 [ gain_ 0.2 [ input i ] ]
        ]
      Nothing -> gain_ 0.02 [ sinOsc_ 440.0 ]
  ]</code></pre>

  @microphone@
  </section>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))(D.value)(sN)({microphone:M(ht(e)(t)(function(n){return ov(!0)(!1)})(function(n){return function(a){return pt(n)([function(){if(a.microphone instanceof T)return au(function(u){return Ft(nt)(1)([B_(P_)(a.microphone.value0),Do(Je)(.1)([Ft(nt)(.2)([u])])])});if(a.microphone instanceof L)return Ft(nt)(.02)([Bh(cc)(440)]);throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Microphone (line 44, column 15 - line 49, column 56): "+[a.microphone.constructor.name])}()])}}))})}}};var mN=function(){return D.value}(),VE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
<h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <pre><code>\\buf -> run2_
  [
    notch_ { frequency: 400.0, q: 1.0 }
    $ pure $ notch_ { frequency: 880.0, q: 5.0 }
    $ pure $ notch_ { frequency: 1200.0, q: 10.0 }
    $ pure $ notch_ { frequency: 2000.0, q: 20.0 }
    $ pure $ notch_ { frequency: 3000.0, q: 30.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~notch~
  </section>
`}})()()(P()(G)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))(mN)({notch:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Zf(Qf(bt(dt()(q(q(Dt)(Gf)()()()({reflectSymbol:function(){return"q"}}))(Jf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:400,q:1})(f(ce)(Zf(Qf(bt(dt()(q(q(Dt)(Gf)()()()({reflectSymbol:function(){return"q"}}))(Jf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:880,q:5})(f(ce)(Zf(Qf(bt(dt()(q(q(Dt)(Gf)()()()({reflectSymbol:function(){return"q"}}))(Jf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:1200,q:10})(f(ce)(Zf(Qf(bt(dt()(q(q(Dt)(Gf)()()()({reflectSymbol:function(){return"q"}}))(Jf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:2e3,q:20})(f(ce)(Zf(Qf(bt(dt()(q(q(Dt)(Gf)()()()({reflectSymbol:function(){return"q"}}))(Jf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:3e3,q:30})(f(ce)(ar(br)(a)(it())))))))))))])}}))})}}};var dN=function(){return D.value}(),GE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

  <pre><code>\\buf -> run2_
  [
    peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~peaking~
  </section>
`}})()()(P()(G)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))(dN)({peaking:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([tl(Xf(bt(dt()(q(q(q(Dt)(Hf)()()()({reflectSymbol:function(){return"q"}}))(zf)()()()({reflectSymbol:function(){return"gain"}}))(Vf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:400,q:1,gain:-20})(f(ce)(tl(Xf(bt(dt()(q(q(q(Dt)(Hf)()()()({reflectSymbol:function(){return"q"}}))(zf)()()()({reflectSymbol:function(){return"gain"}}))(Vf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:880,q:5,gain:20})(f(ce)(tl(Xf(bt(dt()(q(q(q(Dt)(Hf)()()()({reflectSymbol:function(){return"q"}}))(zf)()()()({reflectSymbol:function(){return"gain"}}))(Vf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:1200,q:10,gain:-20})(f(ce)(tl(Xf(bt(dt()(q(q(q(Dt)(Hf)()()()({reflectSymbol:function(){return"q"}}))(zf)()()()({reflectSymbol:function(){return"gain"}}))(Vf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:2e3,q:20,gain:20})(f(ce)(tl(Xf(bt(dt()(q(q(q(Dt)(Hf)()()()({reflectSymbol:function(){return"q"}}))(zf)()()()({reflectSymbol:function(){return"gain"}}))(Vf)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:3e3,q:30,gain:-20})(f(ce)(ar(br)(a)(it())))))))))))])}}))})}}};var yN=function(){return D.value}(),JE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="periodic">Periodic wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">periodic wave oscillator</a> plays back a custom periodic waveform at a given frequency. The custom waveform must be set as part of the initialization and can be changed after initialization. Note that the change will not go into effect if the oscillator is on: it must be turned off and on again.</p>

  <pre><code>\\buf -> run2_
  [
    gain_ 0.2
      [
        periodicOsc
          { frequency: 140.0
          , spec:
              ( (0.1 +> 0.2 +> 0.3 +> 0.4 +> empty)
                  /\\ (0.4 +> 0.3 +> 0.2 +> 0.1 +> empty)
              )
          }
          bangOn
      ]
  ]</code></pre>

  ~periodic~
  </section>
`}})()()(P()(G)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(yN)({periodic:M(ht(e)(t)(function(n){return f(da)(void 0)})(function(n){return function(a){return pt(n)([Ft(nt)(.2)([Ti(Ei(bt(dt()(q(q(Dt)(hi(gi(ea)))()()()({reflectSymbol:function(){return"spec"}}))(Ci)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:140,spec:new Q(xr(Tr(ea)()(_n)()(za))(.1)(xr(Tr(Cu)()(pn)()(_n))(.2)(xr(Tr(hu)()(sn)()(pn))(.3)(xr(Tr(Eu)()(Tu)()(sn))(.4)(Hu)))),xr(Tr(ea)()(_n)()(za))(.4)(xr(Tr(Cu)()(pn)()(_n))(.3)(xr(Tr(hu)()(sn)()(pn))(.2)(xr(Tr(Eu)()(Tu)()(sn))(.1)(Hu)))))})(it())])])}}))})}}};var kN=function(){return D.value}(),jE=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="playbuf">Playing a buffer</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">Playback from a buffer</a> is one of the bread-and-butter operations in Web Audio (or any audio). The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start time and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [
    playBuf
      { buffer
      , duration: 3.0
      , bufferOffset: 4.2
      }
      bangOn
  ]
</code></pre>

  ~playBuf~
  </section>
`}})()()(P()(G)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))(kN)({playBuf:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return pt(n)([qn(L_(bt(dt()(q(q(q(Dt)(uh)()()()({reflectSymbol:function(){return"duration"}}))(ah)()()()({reflectSymbol:function(){return"bufferOffset"}}))(I_)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())))({buffer:a,duration:3,bufferOffset:4.2})(it())])}}))})}}};var Eb=function(){function t(){}return t.value=new t,t}();var XE={attr:function(t){return function(r){return b({key:"controls",value:B(r)})}}};var Tb=function(){function t(){}return t.value=new t,t}();var QE={attr:function(t){return function(r){return b({key:"src",value:B(r)})}}};var Sb=function(t){return function(r){return new F(z("audio")(t)(N(r)))}};var rf=function(){function t(){this.head=null,this.last=null,this.size=0}function r(l,s){this.queue=l,this.value=s,this.next=null,this.prev=null}function e(l){this.draining=!1,this.error=null,this.value=l,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(l){try{l()}catch(s){setTimeout(function(){throw s},0)}}function u(l,s){var v=new r(l,s);switch(l.size){case 0:l.head=v;break;case 1:v.prev=l.head,l.head.next=v,l.last=v;break;default:v.prev=l.last,l.last.next=v,l.last=v}return l.size++,v}function o(l){var s;switch(l.size){case 0:return null;case 1:s=l.head,l.head=null;break;case 2:s=l.last,l.head.next=null,l.last=null;break;default:s=l.last,l.last=s.prev,l.last.next=null}return s.prev=null,s.queue=null,l.size--,s.value}function i(l){var s;switch(l.size){case 0:return null;case 1:s=l.head,l.head=null;break;case 2:s=l.head,l.last.prev=null,l.head=l.last,l.last=null;break;default:s=l.head,l.head=s.next,l.head.prev=null}return s.next=null,s.queue=null,l.size--,s.value}function _(l){if(l.queue!==null){if(l.queue.last===l){o(l.queue);return}if(l.queue.head===l){i(l.queue);return}l.prev&&(l.prev.next=l.next),l.next&&(l.next.prev=l.prev),l.queue.size--,l.queue=null,l.value=null,l.next=null,l.prev=null}}function m(l,s){if(!s.draining){var v=s.puts,c=s.takes,d=s.reads,rt,Z,Vt,zt,Jr;for(s.draining=!0;;){if(rt=null,Z=null,Vt=null,zt=s.value,Jr=d.size,s.error!==null){for(zt=l.left(s.error);rt=i(v);)a(rt.cb(zt));for(;Z=i(d);)a(Z(zt));for(;Vt=i(c);)a(Vt(zt));break}if(zt===n&&(rt=i(v))&&(s.value=zt=rt.value),zt!==n){for(Vt=i(c);Jr--&&(Z=i(d));)a(Z(l.right(zt)));Vt!==null&&(s.value=n,a(Vt(l.right(zt))))}if(rt!==null&&a(rt.cb(l.right(void 0))),s.value===n&&v.size===0||s.value!==n&&c.size===0)break}s.draining=!1}}return e.EMPTY=n,e.putLast=u,e.takeLast=o,e.takeHead=i,e.deleteCell=_,e.drainVar=m,e}();function xb(){return new rf(rf.EMPTY)}function KE(t,r,e){return function(){return e.value===rf.EMPTY&&e.error===null?(e.value=r,rf.drainVar(t,e),!0):!1}}function YE(t,r){return function(){var e=r.value;return e===rf.EMPTY?t.nothing:(r.value=rf.EMPTY,rf.drainVar(t,r),t.just(e))}}var SN=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),xN=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),FN=function(){function t(){}return t.value=new t,t}();var ZE=function(){return{left:Qt.create,right:Kt.create,nothing:L.value,just:T.create,killed:SN.create,filled:xN.create,empty:FN.value}}();var t0=function(t){return function(r){return KE(ZE,t,r)}};var r0=function(t){return YE(ZE,t)};var $N=function(t){return function(r){return function(e){return function(n){return ud(t)(n)(B_(r)(e))}}}},wN=function(){return D.value}(),e0=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(P()(G)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))(wN)({recorder:M(Ve(function(n){return function(a){var u=Gl(bu)(j(K))(a),o=Gl(bu)(j(K))(function(l){return l.left}(u)),i=function(l){return l.right}(o),_=ul($)(g)(e)(function(l){return l.right}(u)),m=function(l){return l.left}(o);return Lr([dn(O($)(f(g)(J(Sf)(Bt.value)("cursor: pointer;")))(p(k)(function(l){return J(fe)(ue.value)(Gr(E(function(){if(l.e instanceof jo)return f(ft)(void 0);if(l.e instanceof Xo)return X(st)(X(st)(X(st)(l.e.value0)(t(f(ft)(void 0))))(kn(ft)(jr)(l.rec)(function(){var s=A_(aD);return function(v){return s(av(v))}}())))(n(Kt.create(xu.value)));if(l.e instanceof xu)return function(){l.cncl();var v=xb();n(new Kt(jo.value))();var c=yo(ct(Fe)(p(Si)(function(d){return d.microphone})(ov(!0)(!1)))(function(d){return le(xe)(function(){var Z=Nt(f(ft)(f(ft)(void 0)))(function(Vt){return function(){var Jr=na(te)(),ie=fc(Jr)(),ut=hn(Cn)(Oe(0))(),It=Xc([$N(WD)(P_)(Vt)(function(Et){return function(){return n(new Qt(new Kt(Et)))(),tr(R)(t0(Et)(v))(),kE("audio/ogg; codecs=opus")(function(Hn){return n(Qt.create(Qt.create(Hn)))})(Et)()}})])(Zc(ut)),Cr=Re(It)(function(Et){return Et(ie)})();return function(){Cr(),ct(Yn)(r0(v))($e(ft)(jr)(function(){var Hn=A_(aD);return function(Be){return Hn(av(Be))}}()))();var ee=Z_(te)(Jr)();return On(ft)(ee!=="closed")(bn(te)(Jr))()}}})(d)();return n(new Kt(new Xo(Z)))(),Z})}))();return t(function(){return n(Kt.create(xu.value))(),Jo(Fi(c))()})(),void 0};throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 66, column 47 - line 110, column 52): "+[l.e.constructor.name])}())))})(En(Pt)(O($)(f(g)(L.value))(p(k)(T.create)(i)))(p(k)(nf)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(l){return l.value0})(e)))(p(k)(function(l){return function(s){return function(v){return{e:l,cncl:s,rec:v}}}})(_)))))))([tn(p(k)(function(l){if(l instanceof xu)return"Turn on";if(l instanceof jo)return"Loading...";if(l instanceof Xo)return"Turn off";throw new Error("Failed pattern match at Ocarina.Example.Docs.AudioUnits.Recorder (line 121, column 31 - line 124, column 56): "+[l.constructor.name])})(_))]),Lr([Sb(O($)(f(g)(J(XE)(Eb.value)("true")))(O($)(f(g)(J(zm)(Bt.value)("display:none;")))(O($)(p(k)(function(l){return J(QE)(Tb.value)(l)})(m))(p(k)(E(J(zm)(Bt.value)("display:block;")))(m)))))([])])])}}))})}}};var PN=function(){return D.value}(),n0=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(P()(G)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(PN)({periodic:M(ht(e)(t)(function(n){return f(da)(void 0)})(function(n){return function(a){return pt(n)([Ft(nt)(.2)([Lh(rh)(448)(it())])])}}))})}}};var RN=function(){return D.value}(),a0=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(P()(G)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(RN)({periodic:M(ht(e)(t)(function(n){return f(da)(void 0)})(function(n){return function(a){return pt(n)([Ft(nt)(.2)([Qc(cc)(448)(it())])])}}))})}}};var LN=function(){return D.value}(),u0=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(P()(G)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(LN)({periodic:M(ht(e)(t)(function(n){return f(da)(void 0)})(function(n){return function(a){return pt(n)([Ft(nt)(.2)([U_(qf)(448)(it())])])}}))})}}};var UN=function(){return D.value}(),o0=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(P()(G)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))(UN)({pan:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return pt(n)([Vh(th)(1)([ar(br)(a)(it())])])}}))})}}};var qN=function(){return D.value}(),i0=St({reflectType:function(){return`<ul>
  <li><a href="#allpass">All-pass filter</a></li>
  <li><a href="#analyser">Analyser</a></li>
  <li><a href="#bandpass">Bandpass filter</a></li>
  <li><a href="#constant">Constant value</a></li>
  <li><a href="#compression">Compression</a></li>
  <li><a href="#convolution">Convolution</a></li>
  <li><a href="#delay">Delay</a></li>
  <li><a href="#gain">Gain</a></li>
  <li><a href="#highpass">Highpass filter</a></li>
  <li><a href="#highshelf">Highshelf filter</a></li>
  <li><a href="#iir">IIR filter</a></li>
  <li><a href="#loopbuf">Looping buffer</a></li>
  <li><a href="#lowpass">Lowpass filter</a></li>
  <li><a href="#lowshelf">Lowshelf filter</a></li>
  <!--<li><a href="#media">Media element</a></li>-->
  <li><a href="#microphone">Microphone</a></li>
  <li><a href="#notch">Notch filter</a></li>
  <!--<li><a href="#panner">Panner</a></li>-->
  <li><a href="#peaking">Peaking filter</a></li>
  <li><a href="#periodic">Periodic wave oscillator</a></li>
  <li><a href="#playbuf">Playing a buffer</a></li>
  <li><a href="#recorder">Recorder</a></li>
  <li><a href="#sawtooth">Sawtooth wave oscillator</a></li>
  <li><a href="#sine">Sine wave oscillator</a></li>
  <li><a href="#square">Square wave oscillator</a></li>
  <li><a href="#pan">Stereo panner</a></li>
  <li><a href="#triangle">Triangle wave oscillator</a></li>
  <li><a href="#waveshaper">Waveshaper</a></li>
    </ul>
`}})()()(G)(qN)({});var zN=function(){return D.value}(),c0=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(P()(G)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(zN)({periodic:M(ht(e)(t)(function(n){return f(da)(void 0)})(function(n){return function(a){return pt(n)([Ft(nt)(.2)([tv(Vs)(448)(it())])])}}))})}}};var GN=function(){return D.value}(),f0=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(P()(P()(G)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(GN)({code:M(Zr(`do
  let
    makeDistortionCurve :: Number -> Array Number
    makeDistortionCurve k =
      map
        ( \\i ->
            let
              x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
            in
              (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
        )
        (0 .. (n_samples - 1))
      where
      n_samples = 44100

      deg = pi / 180.0
  run2_
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(o){var i=ec/180;return p(hr)(function(_){var m=Ur(_)*2/Ur(44100)-1;return(3+o)*m*20*i/(ec+o*Iv(Ra)(kc)(m))})(un(0)(44099))};return pt(n)([Gh($h)(vb(u(400)))([ar(br)(a)(it())])])}}))})}}};var jN=function(){return D.value}(),l0=function(t){return function(r){return function(e){return function(n){var a=X(st)(r(Yc.value))(an),u=Ma(t)(e);return St({reflectType:function(){return`<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Ocarina. There are only two omissions:</p>
    <ul>
      <li>Audio Worklet Nodes</li>
      <li>Multi-channel audio</li>
    </ul>
    <p>Both of these will be covered in later sections.</p>

  <p>
    This section is long and should be read like those passages in the Bible that list who was the child of who: DILIGENTLY AND COPIOUSLY. That said, if you want to skip around, here's a table of contents.
  </p>
  ~toc~
  <p>And now, without further ado... (~drumroll~) Here are some audio nodes!</p>

  ~allpass~
  ~analyser~
  ~bandpass~
  ~constant~
  ~compression~
  ~convolution~
  ~delay~
  ~gain~
  ~highpass~
  ~highshelf~
  ~iirFilter~
  ~loopBuf~
  ~lowpass~
  ~lowshelf~
  ~microphone~
  ~notch~
  ~peaking~
  ~periodicOsc~
  ~playBuf~
  ~recorder~
  ~sawtoothOsc~
  ~sinOsc~
  ~squareOsc~
  ~pan~
  ~triangleOsc~
  ~waveShaper~

  <h2>Next steps</h2>
  <p>Phew, that was a lot of audio units! In the next section, we'll make them come alive thanks to the magic of <a ~next~ style="cursor:pointer;">events</a>.</p>
</div>`}})()()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(rn()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(P()(G)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(jN)({drumroll:M(cv("\u{1F941}")(n)(u)(function(o){return At(o)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(o){return function(i){return pt(o)([Ft(nt)(1)([ar(br)(i)(it())])])}})),toc:M(i0),allpass:M(gE(u)(r)(n)),analyser:M(xE(u)(r)(n)),bandpass:M(FE(u)(r)(n)),constant:M(PE(u)(r)(n)),compression:M(OE(u)(r)(n)),convolution:M(IE(u)(r)(n)),delay:M(RE(u)(r)(n)),gain:M(NE(u)(r)(n)),highpass:M(LE(u)(r)(n)),highshelf:M(BE(u)(r)(n)),iirFilter:M(UE(u)(r)(n)),loopBuf:M(WE(u)(r)(n)),lowshelf:M(HE(u)(r)(n)),lowpass:M(qE(u)(r)(n)),notch:M(VE(u)(r)(n)),playBuf:M(jE(u)(r)(n)),peaking:M(GE(u)(r)(n)),microphone:M(zE(u)(r)(n)),pan:M(o0(u)(r)(n)),periodicOsc:M(JE(u)(r)(n)),recorder:M(e0(u)(r)(n)),sawtoothOsc:M(n0(u)(r)(n)),sinOsc:M(a0(u)(r)(n)),squareOsc:M(u0(u)(r)(n)),triangleOsc:M(c0(u)(r)(n)),waveShaper:M(f0(u)(r)(n)),next:wa($)(g)(n)(a)})}}}};var Fb=function(){function t(){}return t.value=new t,t}(),_0={attr:function(t){return function(r){return b({key:"checked",value:B(r)})}}};var ko=function(){function t(){}return t.value=new t,t}();var Qo={attr:function(t){return function(r){return b({key:"type",value:B(r)})}}};var go=function(t){return function(r){return new F(z("input")(t)(N(r)))}};var YN=function(t){return t},vv=function(t){return function(r){return function(e){return si(t)(O(t.Alternative0().Plus1().Alt0())(f(t.Alternative0().Applicative0())(r))(e))}}};var ep=function(t){return function(r){return t(r)}},_c=function(t){return{map:function(r){return function(e){return function(n){return e(p(t)(function(a){return function(u){return a(r(u))}})(n))}}}}},$i=function(t){return function(r){return function(e){return function(n){return ep(p(_c(t))(r)(e))(p(t)(Bi)(n))}}}};var _l=function(t){return $i(t)(E)};var lu=YN;var p0=function(t){return function(r){return function(e){return lu(function(n){return me(t)(O(t.Alternative0().Plus1().Alt0())(f(t.Alternative0().Applicative0())(ep(r)(n)))(p(t.Filterable1().Functor1())(function(a){return ep(a)(n)})(e)))})}}},Ob=function(t){return{apply:function(r){return function(e){return function(n){return e(r(p(t)(Co(ti))(n)))}}},Functor0:function(){return _c(t)}}};var pl=function(t){return function(r){return Sa(function(e){return Re(r)(function(n){return function(){var u=Y_(t)();return e({acTime:u,value:n})()}})})}};var s0=function(t){return function(r){return function(e){var n=function(a){return function(u){return function(o){return function(i){return function(_){return function(m){return function(){var s=Vr(o)();return On(ft)(s)(function(){var c=Y_(t)(),d=Wp(eg(wu(Ra)(u-c-.04)(.01)*1e3))(function(){var Z=Vr(o)();return On(ft)(Z)(function(){return se(u)(_)(),a(u)(),n(a)(u+m)(o)(i)(_)(m)()})()})();return se(new T(d))(i)()})()}}}}}}};return Sa(function(a){return function(){var o=Nr(!0)(),i=Nr(L.value)(),_=Y_(t)(),m=Nr(_+r)();n(a)(r)(o)(i)(m)(r)();var l=Re(e)(function(s){return function(){ct(Yn)(Vr(i))($e(ft)(jr)(Yl))();var c=Vr(m)();return n(a)(c+s)(o)(i)(m)(s)()}})();return X(st)(X(st)(l)(se(!1)(o)))(ct(Yn)(Vr(i))($e(ft)(jr)(Yl)))}})}}};var Pa=function(t){return function(r){return function(e){return function(n){return function(a){var u=e===t||n===r;if(u)return r;var o=(n-r)/(e-t),i=r-o*t;return o*a+i}}}}};var ZN=function(){return D.value}(),v0=function(t){return function(r){return function(e){return function(n){return St({reflectType:function(){return`<section>
  <h2>Fold</h2>

  <p>The type of <code>fold</code> is:</p>

  <pre><code>fold
    :: forall event a b
    . IsEvent event
    => (a -> b -> b)
    -> event a
    -> b
    -> event b</code></pre>

  <p>Fold starts with some initial state <code>b</code> and, based on incoming events, allows you to change the state.</p>

  <p>One way <code>fold</code> is useful is to retain when certain actions happen. In the following example, we use <code>requestAnimationFrame</code> to animate the audio and we use four <code>fold</code>-s to store the ambitus and velocity of both vibrato and tremolo.</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p><code>fold</code> is so powerful because it allows us to localize state to <i>any</i> event. In the example above, instead of having a global state, our two folds allow for two <i>ad hoc</i> local states.</p>

</section>`}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(ZN)({txt:M(Zr(`module Main where

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
import Ocarina.Control (gain, periodicOsc)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Math (calcSlope)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type Cbx = V (cbx0 :: Unit, cbx1 :: Unit, cbx2 :: Unit, cbx3 :: Unit)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , cbx :: Cbx
  )

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = pure unit <|> event.startStop.start
        stopE = event.startStop.stop
        chkState e = step false $ fold (const not) e false
        cbx0 = chkState event.cbx.cbx0
        cbx1 = chkState event.cbx.cbx1
        cbx2 = chkState event.cbx.cbx2
        cbx3 = chkState event.cbx.cbx3
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                stopE <#> (_ *> push.startStop.start unit)
                startE $> do
                  ctx <- context
                  c0h <- constant0Hack ctx
                  let
                    cevt fast b tm = mapAccum
                      ( \\(oo /\\ act) (pact /\\ pt) ->
                          let
                            tn = pt +
                              ( (act - pact) *
                                  (if oo then fast else 1.0)
                              )
                          in
                            ((act /\\ tn) /\\ tn)
                      )
                      (sampleBy (/\\) b tm)
                      (0.0 /\\ 0.0)

                  r <- run2e ctx
                    ( memoize
                        ( map (add 0.04 <<< _.acTime)
                            $ withACTime ctx animationFrameEvent
                        )
                        \\acTime ->
                          let
                            ev0 = cevt 8.0 cbx0 acTime
                            ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime
                            ev2 = cevt 4.0 cbx2 acTime
                            ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime
                            evs f a = sampleOn acTime
                              $ map ($)
                              $ sampleOn a
                              $ { f: _, a: _, t: _ } <$> f
                          in
                            [ gain 0.0
                                ( evs ev0 ev1 <#> \\{ f, a, t } -> P.gain $ AudioNumeric
                                    { n: calcSlope 1.0 0.01 4.0 0.15 a * sin (pi * f) + 0.15
                                    , o: t
                                    , t: _linear
                                    }
                                )
                                [ periodicOsc
                                    { frequency: 325.6
                                    , spec: (0.3 +> -0.1 +> 0.7 +> -0.4 +> V.empty)
                                        /\\ (0.6 +> 0.3 +> 0.2 +> 0.0 +> V.empty)
                                    }
                                    ( OneOf.do
                                        bangOn
                                        evs ev2 ev3 <#> \\{ f, a, t } -> P.frequency
                                          $ AudioNumeric
                                              { n: 325.6 +
                                                  (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                              , o: t
                                              , t: _linear
                                              }
                                    )
                                ]
                            ]
                    )
                  push.startStop.stop (r *> c0h *> close ctx)
            )
            [ text OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        , D.div
            ( O.oneOfMap (map (attr D.Style)) O.do
                stopE $> "display:block;"
                startE $> "display:none;"
            )
            ( map
                ( \\e -> D.input
                    ( OneOf.do
                        pure (D.Xtype := "checkbox")
                        pure (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:M(Wu()(Lu({reflectSymbol:function(){return"cbx"}})()()()(Yr({reflectSymbol:function(){return"cbx0"}})()()(Yr({reflectSymbol:function(){return"cbx1"}})()()(Yr({reflectSymbol:function(){return"cbx2"}})()()(Yr({reflectSymbol:function(){return"cbx3"}})()()(Pn)()())()())()())()())(Lu({reflectSymbol:function(){return"startStop"}})()()()(Yr({reflectSymbol:function(){return"start"}})()()(Yr({reflectSymbol:function(){return"stop"}})()()(Pn)()())()())(Pn)()())()())(D.value)(function(a){return function(u){var o=O($)(f(g)(void 0))(u.startStop.start),i=function(v){return vv(Pt)(!1)(Ru(Pt)(E(ru(Ba)))(v)(!1))},_=i(u.cbx.cbx3),m=i(u.cbx.cbx2),l=i(u.cbx.cbx1),s=i(u.cbx.cbx0);return Lr([dn(on(Lt)(h)(p(k)(function(){var v=J(fe)(ue.value);return function(c){return v(Gr(E(c)))}}()))([Wr(k)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(v){return v.value0})(n)))(V(k)(o)(j(K))))(function(v){return function(){v();var d=na(te)(),rt=Gu(te)(d)(),Z=function(Jr){return function(ie){return function(ut){return Zl(Pt)(function(It){return function(Cr){var Et=Cr.value1+(It.value1-Cr.value0)*function(){return It.value0?Jr:1}();return new Q(new Q(It.value1,Et),Et)}})($i(k)(Q.create)(ie)(ut))(new Q(0,0))}}},Vt=ol(d)(Mo(p(k)(function(){var Jr=Sr(ka)(.04);return function(ie){return Jr(function(ut){return ut.acTime}(ie))}}())(pl(d)(lc)))(function(Jr){var ie=function(ee){return function(Hn){return si(Pt)(Jr)(p(k)(nf)(si(Pt)(Hn)(p(k)(function(Be){return function(ya){return function(ua){return{f:Be,a:ya,t:ua}}}})(ee))))}},ut=p(k)(function(ee){return ee?4:1})(_l(k)(_)(Jr)),It=Z(4)(m)(Jr),Cr=p(k)(function(ee){return ee?4:1})(_l(k)(l)(Jr)),Et=Z(8)(s)(Jr);return[Le(nt)(0)(Wr(k)(ie(Et)(Cr))(function(ee){return yn()(we)({n:Pa(1)(.01)(4)(.15)(ee.a)*ns(ec*ee.f)+.15,o:ee.t,t:Vo})}))([Ti(Ei(bt(dt()(q(q(Dt)(hi(gi(ea)))()()()({reflectSymbol:function(){return"spec"}}))(Ci)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:325.6,spec:new Q(xr(Tr(ea)()(_n)()(za))(.3)(xr(Tr(Cu)()(pn)()(_n))(-.1)(xr(Tr(hu)()(sn)()(pn))(.7)(xr(Tr(Eu)()(Tu)()(sn))(-.4)(Hu)))),xr(Tr(ea)()(_n)()(za))(.6)(xr(Tr(Cu)()(pn)()(_n))(.3)(xr(Tr(hu)()(sn)()(pn))(.2)(xr(Tr(Eu)()(Tu)()(sn))(0)(Hu)))))})(be(Lt)(h)([it(),Wr(k)(ie(It)(ut))(function(ee){return Ao()(we)({n:325.6+Pa(1)(3)(4)(15.5)(ee.a)*ns(ec*ee.f),o:ee.t,t:Vo})})]))])]}))(),zt=X(st)(X(st)(Vt)(rt))(bn(te)(d));return t(X(st)(zt)(a.startStop.start(void 0)))(),a.startStop.stop(zt)()}}),Wr(k)(u.startStop.stop)(function(v){return X(st)(v)(X(st)(t(f(ft)(void 0)))(a.startStop.start(void 0)))})]))([tn(be(Lt)(h)([V(k)(o)("Turn on"),V(k)(u.startStop.stop)("Turn off")]))]),dr(on(Lt)(h)(p(k)(J(lt)(Bt.value)))([V(k)(u.startStop.stop)("display:block;"),V(k)(o)("display:none;")]))(p(hr)(function(v){return go(be(Lt)(h)([f(g)(J(Qo)(ko.value)("checkbox")),f(g)(J(fe)(ue.value)(Gr(E(v(void 0))))),V(k)(o)(J(_0)(Fb.value)("false"))]))([])})(xv(hr)([function(v){return v.cbx0},function(v){return v.cbx1},function(v){return v.cbx2},function(v){return v.cbx3}])(a.cbx)))])}}))})}}}};var $b={recip:function(t){return 1/t},Ring0:function(){return kc}};var wb=function(t){return function(r){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return r}}}};function sl(t){return function(){return function(r){return t(r)()}}}function vl(t){return function(r){return function(e){return function(n){return function(){return n.addEventListener(t,r,e)}}}}}function ml(t){return function(r){return function(e){return function(n){return function(){return n.removeEventListener(t,r,e)}}}}}function Mb(t){return t.clientX}function Pb(t){return t.clientY}function np(t){return t.button}var ap=xt("MouseEvent");var m0=function(t){return function(r){return Sa(function(e){return Re(r)(function(n){return function(){var u=Vr(t.buttons)();return e({value:n,buttons:u})()}})})}};var D0=function(){var r=Nr(L.value)(),e=Nr(Cm)(),n=p(R)(sD)(Ai)(),a=sl(function(_){return $e(ft)(jr)(function(m){return se(new T({x:Mb(m),y:Pb(m)}))(r)})(ap(_))})(),u=sl(function(_){return $e(ft)(jr)(function(m){return To(JA(ze)(np(m)))(e)})(ap(_))})(),o=sl(function(_){return $e(ft)(jr)(function(m){return To(Up(ze)(np(m)))(e)})(ap(_))})();vl(Ye()("mousemove"))(a)(!1)(n)(),vl(Ye()("mousedown"))(u)(!1)(n)(),vl(Ye()("mouseup"))(o)(!1)(n)();var i=function(){return ml(Ye()("mousemove"))(a)(!1)(n)(),ml(Ye()("mousedown"))(u)(!1)(n)(),ml(Ye()("mouseup"))(o)(!1)(n)()};return{position:r,buttons:e,dispose:i}},d0=Sa(function(t){return function(){var e=p(R)(sD)(Ai)(),n=sl(function(a){return $e(ft)(jr)(function(u){return t(np(u))})(ap(a))})();return vl(Ye()("mousedown"))(n)(!1)(e)(),ml(Ye()("mousedown"))(n)(!1)(e)}});var y0=function(t){return lu(function(r){return p(k)(function(e){return e.value(e.buttons)})(m0(t)(r))})};var Nb=function(t){return t};function dv(){return Date.now()}var z0=function(t){return Sa(function(r){return Re(t)(function(e){return function(){var a=dv();return r({time:a,value:e})()}})})};var IL=lu(function(t){return p(k)(function(r){return r.value(r.time)})(z0(t))}),Bb=p(_c(k))(function(){var t=z_(oE);return function(r){return t(Nb(r))}}())(IL);var NL=function(t){var r=function(u){return function(o){return function(i){return function(_){return function(m){return function(l){return function(s){var v=Sr(o.DivisionRing1().Ring0().Semiring0())(ca(o.DivisionRing1().Ring0().Semiring0()))(ca(o.DivisionRing1().Ring0().Semiring0())),c=function(d){return function(rt){if(d.last instanceof L)return rt;if(d.last instanceof T)return Sr(i)(rt)(_(function(Z){return Yu(o.EuclideanRing0())(An(o.DivisionRing1().Ring0().Semiring0())(Z(Sr(i)(d.last.value0.value1)(d.now.value1)))(pu(o.DivisionRing1().Ring0())(d.now.value0)(d.last.value0.value0)))(v)}));throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[d.constructor.name,rt.constructor.name])}};return lu(function(d){var rt=ep(s)(V(u.Filterable1().Functor1())(d)(j(K))),Z=zp(u)($i(u.Filterable1().Functor1())(Q.create)(l)(rt)),Vt=Ru(u)(c)(Z)(m);return si(u)(Vt)(d)})}}}}}}},e=function(u){return function(o){return r(u)(o)(o.DivisionRing1().Ring0().Semiring0())(function(i){return i(j(K))})}},n=function(u){return function(o){return lu(function(i){return t_(Pt)(function(_){var m=o(vv(Pt)(u)(_));return{input:_l(k)(m)(i),output:si(Pt)(_)(i)}})})}},a=function(u){return function(o){return function(i){if(GA(u))return-8*(o-1)-i*2;if(Qr)return 2*(4-o);throw new Error("Failed pattern match at Ocarina.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[u.constructor.name,o.constructor.name,i.constructor.name])}}};return n(2)(function(u){return e(Pt)(wb(Fl)($b))(2)(p(_c(k))(De())(Bb))(function(){var o=n(10)(function(i){return e(Pt)(wb(Fl)($b))(10)(p(_c(k))(De())(Bb))($t(Ob(k))($t(Ob(k))(p(_c(k))(a)(y0(t)))(u))(i))});return p0(Pt)(o)(V(k)(d0)(o))}())})},LL=function(){return D.value}(),V0=function(t){return function(r){return function(e){return function(n){return St({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(LL)({txt:M(Zr(`module Main

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Behavior (ABehavior, Behavior, behavior, sample, sampleBy, sample_, step, switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Time as Time
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, fix, fold, sampleOn, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (withACTime)
import Ocarina.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

-- \`swell\` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = \u237A - \u03B2s
--   | mouse up   = \u0263 - \u03B4s - \u03B5 ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using \`solve2'\`.
swell :: Mouse -> Behavior Number
swell mouse =
  fixB 2.0 \\b ->
    integral' 2.0 (unwrap <$> Time.seconds)
      let
        db = fixB 10.0 \\db_ ->
          integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
      in
        switcher db (down $> db)
  where
  f bs s ds
    | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
    | otherwise = 2.0 * (4.0 - s)

  fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
  fixB a fn = behavior \\s ->
    fix \\event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOn event s }

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function approximates the integral using the trapezium rule at the
  -- | implicit sampling interval.
  -- |
  -- | The \`Semiring\` \`a\` should be a vector field over the field \`t\`. To represent
  -- | this, the user should provide a _grate_ which lifts a multiplication
  -- | function on \`t\` to a function on \`a\`. Simple examples where \`t ~ a\` can use
  -- | the \`integral'\` function instead.
  integral
    :: forall event a t
     . IsEvent event
    => Field t
    => Semiring a
    => (((a -> t) -> t) -> a)
    -> a
    -> ABehavior event t
    -> ABehavior event a
    -> ABehavior event a
  integral g initial t b =
    behavior \\e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\\) t x)
        z = fold approx y initial
      in
        sampleOn z e
    where
    approx { last: Nothing } s = s
    approx { now: t1 /\\ a1, last: Just (t0 /\\ a0) } s = s + g (\\z -> z (a0 + a1) * (t1 - t0) / two)

    two :: t
    two = one + one

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function is a simpler version of \`integral\` where the function being
  -- | integrated takes values in the same field used to represent time.
  integral'
    :: forall event t
     . IsEvent event
    => Field t
    => t
    -> ABehavior event t
    -> ABehavior event t
    -> ABehavior event t
  integral' = integral (_ $ identity)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ StartStop) \\push event -> do
      let
        startE = pure unit <|> event.start
        stopE = event.stop
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                event.stop <#> (_ *> push.start unit)
                startE $>
                  do
                    ctx <- context
                    c0h <- constant0Hack ctx
                    mouse <- getMouse
                    ri <- randomInt 0 10000
                    let
                      ttap (o /\\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }
                      fund = 90.4
                      spcn = map (_ - 0.5) arbitrary
                      spc' = do
                        a <- spcn
                        b <- spcn
                        c <- spcn
                        d <- spcn
                        pure (a +> b +> c +> d +> V.empty)
                      spc = (/\\) <$> spc' <*> spc'
                      spcs = { s0: _, s1: _, s2: _, s3: _ } <$> spc <*> spc <*> spc <*> spc
                      allSpcs = evalGen spcs { newSeed: mkSeed ri, size: 5 }
                    r <- run2e ctx
                      ( memoize
                          ( map (\\{ acTime, value } -> acTime /\\ value)
                              $ withACTime ctx
                              $ sample_ (swell mouse) animationFrameEvent
                          )
                          \\swm ->
                            [ gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.4) $ 0.5 * (x - 1.0)) <$> swm
                                )
                                [ lowpass_ { frequency: fund, q: 20.0 }
                                    [ squareOsc_ fund ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.2) $ 0.4 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 4.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: (fund * 3.02)
                                        , spec: allSpcs.s0
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 3.02 + 14.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.1) $ 0.2 * (x - 6.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 6.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 5.07
                                        , spec: allSpcs.s1
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 5.07 + 18.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.2 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 8.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 7.13
                                        , spec: allSpcs.s2
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 7.13 + 32.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.1 * (x - 7.0)) <$> swm
                                )
                                [ periodicOsc
                                    { frequency: fund * 9.14
                                    , spec: allSpcs.s3
                                    }
                                    ( bangOn <|>
                                        ( P.frequency
                                            <<< ttap
                                            <<< second (\\x -> fund * 9.14 + 31.0 * (x - 1.0)) <$> swm
                                        )
                                    )
                                ]
                            ]
                      )
                    push.stop (r *> c0h *> close ctx)
            )
            [ text $ OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        ]
  )`)),empl:M(Wu()(Yr({reflectSymbol:function(){return"start"}})()()(Yr({reflectSymbol:function(){return"stop"}})()()(Pn)()())()())(D.value)(function(a){return function(u){var o=O($)(f(g)(void 0))(u.start);return Lr([dn(on(Lt)(h)(p(k)(function(){var i=J(fe)(ue.value);return function(_){return i(Gr(E(_)))}}()))([Wr(k)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(i){return i.value0})(n)))(V(k)(o)(j(K))))(function(i){return function(){i();var m=na(te)(),l=Gu(te)(m)(),s=D0(),v=p_(0)(1e4)(),c=function(ut){return{o:ut.value0+.04,n:ut.value1,t:Vo}},d=p(io)(function(ut){return ut-.5})(Wc(wg)),rt=ct(Bc)(d)(function(ut){return ct(Bc)(d)(function(It){return ct(Bc)(d)(function(Cr){return ct(Bc)(d)(function(Et){return f(E_)(xr(Tr(ea)()(_n)()(za))(ut)(xr(Tr(Cu)()(pn)()(_n))(It)(xr(Tr(hu)()(sn)()(pn))(Cr)(xr(Tr(Eu)()(Tu)()(sn))(Et)(Hu)))))})})})}),Z=$t(Uc)(p(io)(Q.create)(rt))(rt),Vt=$t(Uc)($t(Uc)($t(Uc)(p(io)(function(ut){return function(It){return function(Cr){return function(Et){return{s0:ut,s1:It,s2:Cr,s3:Et}}}}})(Z))(Z))(Z))(Z),zt=ic(Vt)({newSeed:nc(v),size:5}),Jr=ol(m)(Mo(p(k)(function(ut){return new Q(ut.acTime,ut.value)})(pl(m)(_l(k)(NL(s))(lc))))(function(ut){return[Le(nt)(0)(p(k)(function(){var It=yn()(we),Cr=Un(vn)(function(Et){return wu(Ra)(-.4)(.5*(Et-1))});return function(Et){return It(c(Cr(Et)))}}())(ut))([Yf(td(bt(dt()(q(q(Dt)(fh)()()()({reflectSymbol:function(){return"q"}}))(qD)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4,q:20})([Uh(qf)(90.4)])]),Le(nt)(0)(p(k)(function(){var It=yn()(we),Cr=Un(vn)(function(Et){return wu(Ra)(-.2)(.4*(Et-3))});return function(Et){return It(c(Cr(Et)))}}())(ut))([Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4*4,q:20})([Ti(Ei(bt(dt()(q(q(Dt)(hi(gi(ea)))()()()({reflectSymbol:function(){return"spec"}}))(Ci)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4*3.02,spec:zt.s0})(O($)(it())(p(k)(function(){var It=Ao()(we),Cr=Un(vn)(function(Et){return 90.4*3.02+14*(Et-1)});return function(Et){return It(c(Cr(Et)))}}())(ut)))])]),Le(nt)(0)(p(k)(function(){var It=yn()(we),Cr=Un(vn)(function(Et){return wu(Ra)(-.1)(.2*(Et-6))});return function(Et){return It(c(Cr(Et)))}}())(ut))([Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4*6,q:20})([Ti(Ei(bt(dt()(q(q(Dt)(hi(gi(ea)))()()()({reflectSymbol:function(){return"spec"}}))(Ci)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4*5.07,spec:zt.s1})(O($)(it())(p(k)(function(){var It=Ao()(we),Cr=Un(vn)(function(Et){return 90.4*5.07+18*(Et-1)});return function(Et){return It(c(Cr(Et)))}}())(ut)))])]),Le(nt)(0)(p(k)(function(){var It=yn()(we),Cr=Un(vn)(function(Et){return wu(Ra)(0)(.2*(Et-3))});return function(Et){return It(c(Cr(Et)))}}())(ut))([Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4*8,q:20})([Ti(Ei(bt(dt()(q(q(Dt)(hi(gi(ea)))()()()({reflectSymbol:function(){return"spec"}}))(Ci)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4*7.13,spec:zt.s2})(O($)(it())(p(k)(function(){var It=Ao()(we),Cr=Un(vn)(function(Et){return 90.4*7.13+32*(Et-1)});return function(Et){return It(c(Cr(Et)))}}())(ut)))])]),Le(nt)(0)(p(k)(function(){var It=yn()(we),Cr=Un(vn)(function(Et){return wu(Ra)(0)(.1*(Et-7))});return function(Et){return It(c(Cr(Et)))}}())(ut))([Ti(Ei(bt(dt()(q(q(Dt)(hi(gi(ea)))()()()({reflectSymbol:function(){return"spec"}}))(Ci)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:90.4*9.14,spec:zt.s3})(O($)(it())(p(k)(function(){var It=Ao()(we),Cr=Un(vn)(function(Et){return 90.4*9.14+31*(Et-1)});return function(Et){return It(c(Cr(Et)))}}())(ut)))])]}))(),ie=X(st)(X(st)(Jr)(l))(bn(te)(m));return t(X(st)(ie)(a.start(void 0)))(),a.stop(ie)()}}),Wr(k)(u.stop)(function(i){return X(st)(i)(X(st)(t(f(ft)(void 0)))(a.start(void 0)))})]))([tn(be(Lt)(h)([V(k)(o)("Turn on"),V(k)(u.stop)("Turn off")]))])])}}))})}}}};var UL=function(){return D.value}(),G0=function(t){return function(r){return function(e){return function(n){var a=Ma(t)(e);return St({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(P()(P()(rn()(G)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}}))(UL)({next:wa($)(g)(n)(X(st)(r(Q_.value))(an)),fold:M(v0(a)(r)(e)(n)),fix:M(V0(a)(r)(e)(n))})}}}};var qL=function(){function t(){}return t.value=new t,t}(),J0=function(){function t(){}return t.value=new t,t}(),Ub=function(){function t(r){this.value0=r}return t.create=function(r){return new t(r)},t}(),HL=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import FRP.Event (bus)

import Math (pow)
import Ocarina.Control (gain_, gain, sinOsc)
import Ocarina.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = pure $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = pure $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = pure $ P.gain
  $ AudioEnvelope
      { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
      , d: 0.8
      , o
      }

-- a single cell with four oscillators,
-- each of which have the envelope applied
cell = lcmap toNumber \\i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 \`pow\` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 \`pow\` i)))
    env' x = env (x + 0.3 * (i * (1.005 \`pow\` i)))
    strand x y =
      gain 0.0 (env' x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

main :: Effect Unit
main = runInBody1 (bus \\push -> lcmap (pure Init <|> _) \\event ->
  D.div_
    [ D.button
        ( event <#>
            \\e -> D.OnClick := cb
              ( const $ case e of
                  Stop u -> u *> push Start
                  _ -> do
                    r <- run2_
                      [ gain_ 1.0
                          -- we create 100 cells
                          $ join
                          $ cell <$> 0 .. 100
                      ]
                    push $ Stop r
              )
        )
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ])
`;var zL=function(){return D.value}(),VL=function(t){return function(r){return function(e){return f(t)(tf(r)(Vc)({x:UD,o:e}))}}},GL=function(t){return function(r){return function(e){return f(t)(tf(r)(Vc)({x:KC,o:e}))}}},JL=qa(In)(Ur)(function(t){var r=function(a){return O($)(VL(g)()(a+.27*(t*rc(1.005)(t))))(GL(g)()(a+3+.3*(t*rc(1.005)(t))))},e=function(a){return f(g)(yn()(Sn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*rc(1.005)(t)),d:.8}))},n=function(a){return function(u){return Le(nt)(0)(e(a))([Qc(cc)(200+t*u)(r(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),j0=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2>Example 1: Hello events</h2>

  <p>Let's say hi to events! The simplest of events, which we've seen already, are the ones that occur <span style="font-weight:800;">now</span>, that is to say, immediately upon subscription. You create those types of events using <code>bang</code>. In this section, we'll use <code>bang</code> to set several different types of values:</p>

  <ul>
    <li><code>AudioEnvelope</code> to create an envelope for the gain node. To construct one, use a record with the following parameters:<ul><li><code>p</code>: a list of numbers that will be interpolated over.</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li><li><code>d</code>: the duration of the envelope.</li></ul></li>
    <li><code>AudioOnOff</code> to turn the sine-wave oscillator on and off. To construct one, use a record with the following parameters:<ul><li><code>n</code>: an enum with the value <code>_on</code>, <code>_off</code> or <code>_onOff</code> (more on this in <a href="#example3">Example 3</a> below).</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li></ul></li>
  </ul>

  <p>After that, in the example below, it's functions all the way down. <code>oon</code> and <code>oof</code> create our on/off events, <code>env</code> creates our gain envelope, <code>ooo'</code> and <code>env'</code> specialize these envelopes to a specific point in time, and <code>cell</code> creates a single cell that we deploy 100 times.</p>

  <p>One important thing to note here is the use of the tie fighter (<code>&lt;|&gt;</code>), aka <code>alt</code>, in the definition of <code>ooo'</code>. The <code>Event</code> type, when <code>alt</code>'d, preserves a before-after relationship of the left and right operands when the operands happen at the same time. This is a bit hackish: the events conceptually happen at the same time, but on our CPU, one has to follow the other. We can use this, however, to make sure that certain events happen in a logical sequence. For example, an <code>off</code> instruction must be issued after an <code>on</code> instruction, which we guarantee by using <code>oon</code> on the left side of the alt. If we did it the other way, the <code>on</code> instruction would be last and we'd wind up with 100 oscillators playing at the same time!</p>

  <p>A last thing to note before the music plays is how scheduling works here. Even though all the events are issued upfront via <code>bang</code>, they schedule things to be played <i>later</i> in the audio context. We'll see more advanced scheduling techniques in the <a href="#example4"><code>requestAnimationFrame</code> example below</a>.</p>

  <pre><code>@txt@</code></pre>

  @ex0@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
`}})({reflectType:function(){return"@"}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(D.value)(zL)({txt:M(Zr(HL)),ex0:M(Ve(function(n){return qa(In)(function(a){return O($)(f(g)(qL.value))(a)})(function(a){return Lr([dn(Wr(k)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(u){return u.value0})(e)))(p(k)(Q.create)(a)))(function(u){return J(fe)(ue.value)(Gr(E(function(){return u.value0 instanceof Ub?X(st)(X(st)(u.value0.value0)(n(J0.value)))(t(f(ft)(void 0))):function(){u.value1();var i=fv([Ft(nt)(1)(ei(ri)(p(hr)(JL)(un(0)(100))))])();return t(X(st)(i)(n(J0.value)))(),n(new Ub(i))()}}())))}))([tn(Wr(k)(a)(function(u){return u instanceof Ub?"Turn off":"Turn on"}))])])})}))})}}};var wi=function(){function t(){}return t.value=new t,t}();var sc={attr:function(t){return function(r){return b({key:"max",value:B(r)})}}};var Mi=function(){function t(){}return t.value=new t,t}();var vc={attr:function(t){return function(r){return b({key:"min",value:B(r)})}}};var Pi=function(){function t(){}return t.value=new t,t}();var mc={attr:function(t){return function(r){return b({key:"input",value:at(r)})}}};var Ii=function(){function t(){}return t.value=new t,t}(),Dc={attr:function(t){return function(r){return b({key:"step",value:B(r)})}}};var Ri=function(){function t(){}return t.value=new t,t}();var dc={attr:function(t){return function(r){return b({key:"value",value:B(r)})}}};var Ko=function(t){return function(r){return function(e){return O(t)(r)(e(void 0))}}};var XL=_g,_u={convert:function(t){return t}},up={convert:function(t){return d_(t)}},Q0=function(t){return t},Wb=function(t){return t.convert},Ja=function(t){return function(r){return function(e){return _t(XL)(d_(r))(Wb(t)(e(void 0)))}}};var op=function(t){return function(r){return function(e){return function(n){return on(pg)(r)(e)(Q0(Wb(t)(n)))}}}};function Y0(t){return t.target}var Dl=function(t){return Ge(Y0(t))};var YL=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (biSampleOn)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> pure unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              OneOf.do
                bangOn
                map
                  (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                  sl0
                map
                  (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                  sl1
                map
                  (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                  (biSampleOn sl2 (add <$> (pure 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap pure O.do
                        D.Xtype := "range"
                        D.Min := "0"
                        D.Max := "100"
                        D.Step := "1"
                        D.Value := "50"
                        D.OnInput := cb
                          ( traverse_
                              (valueAsNumber >=> f)
                              <<< (=<<) fromEventTarget
                              <<< target
                          )
                    )
                    []
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                    start $> (music >>= push.startStop.stop)
                    event.startStop.stop <#>
                      (_ *> push.startStop.start unit)
                )
                [ text OneOf.do
                    start $> "Turn on"
                    event.startStop.stop $> "Turn off"
                ]
            ]
`,ZL=function(){return D.value}(),t1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",Z0=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Ocarina bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>Ocarina.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Ocarina parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(P()(P()(P()(G)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(D.value)(ZL)({wagtxt:M(Zr(`run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ OneOf.do
      bangOn
      (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (pure 0.0 <|> sl1))`)),txt:M(Zr(YL)),ex1:M(Wu()(Lu({reflectSymbol:function(){return"slider"}})()()()(Yr({reflectSymbol:function(){return"s0"}})()()(Yr({reflectSymbol:function(){return"s1"}})()()(Yr({reflectSymbol:function(){return"s2"}})()()(Pn)()())()())()())(Lu({reflectSymbol:function(){return"startStop"}})()()()(Yr({reflectSymbol:function(){return"loading"}})()()(Yr({reflectSymbol:function(){return"start"}})()()(Yr({reflectSymbol:function(){return"stop"}})()()(Pn)()())()())()())(Pn)()())()())(D.value)(function(n){return function(a){var u=O($)(a.startStop.start)(f(g)(void 0)),o=function(i){return ar(jc(bt(dt()(q(q(q(q(Dt)(jf)()()()({reflectSymbol:function(){return"playbackRate"}}))(N_)()()()({reflectSymbol:function(){return"loopStart"}}))(R_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Jc)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())))({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(Ko($)(it())(function(){return Ko($)(p(k)(function(){var _=aa()(zs),m=Pa(0)(.2)(100)(5);return function(l){return _(m(l))}}())(a.slider.s0))(function(){return Ko($)(p(k)(function(){var _=wE(),m=Pa(0)(0)(100)(1.2);return function(l){return _(m(l))}}())(a.slider.s1))(function(){return p(k)(function(){var _=ME(),m=Pa(0)(.05)(100)(1);return function(l){return _(m(l))}}())(En(Pt)(a.slider.s2)(p(k)(Sr(ka))(O($)(f(g)(0))(a.slider.s1))))})})}))};return Lr(_t(Pe)(p(hr)(function(i){return Lr([Zr(i.l),go(op(_u)(h)(f(g))(Ja(_u)(J(Qo)(ko.value)("range"))(function(){return Ja(_u)(J(vc)(Mi.value)("0"))(function(){return Ja(_u)(J(sc)(wi.value)("100"))(function(){return Ja(_u)(J(Dc)(Ii.value)("1"))(function(){return Ja(up)(J(dc)(Ri.value)("50"))(function(){return J(mc)(Pi.value)(Gr(function(){var _=$e(ft)(jr)(af(Yn)(qc)(i.f)),m=Jn(ga)(Rf);return function(l){return _(m(Dl(l)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([dn(op(_u)(h)(p(k)(function(){var i=J(fe)(ue.value);return function(_){return i(Gr(E(_)))}}()))(Ja(_u)(V(k)(a.startStop.loading)(f(ft)(void 0)))(function(){return Ja(up)(Wr(k)(a.startStop.stop)(function(i){return X(st)(i)(X(st)(t(f(ft)(void 0)))(n.startStop.start(void 0)))}))(function(){return Wr(k)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(i){return i.value0})(e)))(V(k)(u)(j(K))))(function(i){return function(){i(),n.startStop.loading(void 0)();var m=yo(ct(Fe)(na(xe))(function(l){return ct(Fe)(Gu(xe)(l))(function(s){return ct(Fe)(At(l)(t1))(function(v){return le(xe)(function(){var d=pt(l)([o(v)])(),rt=X(st)(X(st)(d)(s))(bn(te)(l));return n.startStop.stop(rt)(),rt})})})}))();return t(function(){return n.startStop.start(void 0)(),Jo(Fi(m))()})(),void 0}})})})))([tn(Ko($)(p(k)(E("Turn off"))(a.startStop.stop))(function(){return p(k)(E("Turn on"))(u)}))])]))}}))})}}};var e1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import QualifiedDo.OneOfMap as O
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)

import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (interval)
import Ocarina.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import Ocarina.Interpret (close, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (frequency)
import Ocarina.Properties as P
import Ocarina.Run (run2e)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  )

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        start = event.startStop.start <|> pure unit

        music :: forall lock. _ -> Event (Array (Audible _ lock _))
        music evt' = memoize evt' \\evt -> do
          let
            pitch = map fst evt
            -- to avoid artifacts in the pitch change
            time = map (add 0.01 <<< snd) evt
            e0 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e1 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e2 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            f0 = bangOn <|> frequency <<< cp <$> pitch
          [ fan1 (triangleOsc 0.0 f0) \\ipt _ -> do
              gain_ 2.0
                [ gain 0.0 (P.gain <$> e0)
                    [ bandpass_
                        { frequency: 1000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e1)
                    [ bandpass_
                        { frequency: 2000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e2)
                    [ highpass_
                        { frequency: 4000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                ]
          ]
      D.div_
        [ D.div_
            [ text_ "tempo"
            , D.input
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb
                      ( traverse_
                          (valueAsNumber >=> push.slider)
                          <<< (=<<) fromEventTarget
                          <<< target
                      )
                )
                []
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ start $> do
                    ctx <- context
                    let
                      myIvl = sampleBy Tuple random
                        $ interval ctx 0.91
                        $ map (calcSlope 0.0 0.42 100.0 1.4)
                        $ event.slider
                    r <- run2e ctx (music myIvl)
                    push.startStop.stop (r *> close ctx)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ start $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
  )`,n1=lu(function(t){return Sa(function(r){return Re(t)(function(e){return function(){var a=Uo();return r(e(a))()}})})}),a1=function(){return D.value}(),u1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(Qr)return 587.329536;throw new Error("Failed pattern match at Ocarina.Example.Docs.Events.Ex2 (line 226, column 1 - line 226, column 23): "+[t.constructor.name])},tT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Ocarina comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Ocarina <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in ocarina is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The ocarina interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The ocarina interval needs an audio context to work.</li>
    <li>The ocarina interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))(D.value)(a1)({txt:M(Zr(e1)),ex2:M(Wu()(Yr({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(Yr({reflectSymbol:function(){return"start"}})()()(Yr({reflectSymbol:function(){return"stop"}})()()(Pn)()())()())(Pn)()())()())(D.value)(function(n){return function(a){var u=O($)(a.startStop.start)(f(g)(void 0)),o=function(i){return Mo(i)(function(_){var m=p(k)(function(){var rt=Sr(ka)(.01);return function(Z){return rt(Ke(Z))}}())(_),l=p(k)(Ua)(_),s=O($)(it())(p(k)(function(){var rt=Ao()(zs);return function(Z){return rt(u1(Z))}}())(l)),v=p(k)(function(rt){return qs(function(Z){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:Z}}(rt))})(m),c=p(k)(function(rt){return qs(function(Z){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:Z}}(rt))})(m),d=p(k)(function(rt){return qs(function(Z){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:Z}}(rt))})(m);return[Oa(tv(Vs)(0)(s))(function(rt){return function(Z){return Ft(nt)(2)([Le(nt)(0)(p(k)(yn()(Sn))(d))([Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:1e3,q:20})([rt])]),Le(nt)(0)(p(k)(yn()(Sn))(c))([Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:2e3,q:20})([rt])]),Le(nt)(0)(p(k)(yn()(Sn))(v))([Kf(rd(bt(dt()(q(q(Dt)(ph)()()()({reflectSymbol:function(){return"q"}}))(HD)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:4e3,q:20})([rt])])])}})]})};return Lr([Lr([Zr("tempo"),go(op(_u)(h)(f(g))(Ja(_u)(J(Qo)(ko.value)("range"))(function(){return Ja(_u)(J(vc)(Mi.value)("0"))(function(){return Ja(_u)(J(sc)(wi.value)("100"))(function(){return Ja(_u)(J(Dc)(Ii.value)("1"))(function(){return Ja(up)(J(dc)(Ri.value)("50"))(function(){return J(mc)(Pi.value)(Gr(function(){var i=$e(ft)(jr)(af(Yn)(qc)(n.slider)),_=Jn(ga)(Rf);return function(m){return i(_(Dl(m)))}}()))})})})})})))([])]),dn(on(Lt)(h)(p(k)(function(){var i=J(fe)(ue.value);return function(_){return i(Gr(E(_)))}}()))([Wr(k)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(i){return i.value0})(e)))(V(k)(u)(j(K))))(function(i){return function(){i();var m=na(te)(),l=$i(k)(Q.create)(n1)(s0(m)(.91)(p(k)(Pa(0)(.42)(100)(1.4))(a.slider))),s=ol(m)(o(l))(),v=X(st)(s)(bn(te)(m));return t(X(st)(v)(n.startStop.start(void 0)))(),n.startStop.stop(X(st)(v)(bn(te)(m)))()}}),Wr(k)(a.startStop.stop)(function(i){return X(st)(i)(X(st)(t(f(ft)(void 0)))(n.startStop.start(void 0)))})]))([tn(be(Lt)(h)([V(k)(u)("Turn on"),V(k)(a.startStop.stop)("Turn off")]))])])}}))})}}};var i1=function(){return D.value}(),rT=function(){return ir({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(G)(D.value)(i1)({})}();var f1=function(){return D.value}(),eT=function(){return ir({reflectType:function(){return`<section>
  <h2>Events in Ocarina</h2>
  <p>Ocarina follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

  <pre><code>sinOsc
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels "" C.G_ event payload</code></pre>

  <p>In that defintion, <code>SinOsc</code> is a <code>newtype</code> that is defined as such:</p>

  <pre><code>newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))</code></pre>

  <p>Informally, the <code>sinOsc</code> function listens to an event that contains one of several possible instructions, including changing the frequency and turning the oscillator on or off. Let's see a few more examples:</p>

    <table>
    <tr>
      <th>Audio Unit</th>
      <th>Newtype name</th>
      <th>Variant</th>
    </tr>
    <tr>
      <td><code>delay</code></td>
      <td><code>Delay</code></td>
      <td><code>Variant (delayTime :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>highshelf</code></td>
      <td><code>Highshelf</code></td>
      <td><code>Variant (frequency :: AudioParameter, gain :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>playBuf</code></td>
      <td><code>PlayBuf</code></td>
      <td><code>Variant (playbackRate :: AudioParameter, buffer :: BrowserAudioBuffer, duration :: Maybe Number, bufferOffset :: Number, onOff :: AudioOnOff)</code></td>
    </tr>
  </table>

  <p>In practice, you'll never need to use newtypes presented above. The <code>Ocarina.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


    <table>
    <tr>
      <th>Constructor</th>
      <th>Type</th>
      <th>Receptive Audio Units</th>
    </tr>
    <tr>
      <td><code>bufferOffset</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>delayTime</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>delay</code></li></ul></td>
    </tr>
    <tr>
      <td><code>duration</code></td>
      <td><code>Maybe Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>frequency</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowpass</code></li>
          <li><code>lowshelf</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>gain</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>gain</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowshelf</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>loopEnd</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>loopStart</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>offset</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>onOff</code></td>
      <td><code>AudioOnOff</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
          <li><code>loopBuf</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>playBuf</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>playbackRate</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>loopBuf</code></li><li>playBuf</li></ul></td>
    </tr>
    <tr>
      <td><code>q</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>lowpass</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
  </table>

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Ocarina can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Ocarina:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Ocarina. Idiomatic Ocarina projects use functional reactive programming as a way to "steer" web audio, and Ocarina aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(G)(D.value)(f1)({})}();var _1=function(){return D.value}(),nT=function(){return ir({reflectType:function(){return`<section>

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Behavior</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype ABehavior event a =
  ABehavior (forall b. event (a -> b) -> event b)
type Behavior = ABehavior Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Ocarina, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Ocarina, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(G)(D.value)(_1)({})}();var s1=function(){return D.value}(),aT=function(t){return function(r){return function(e){return function(n){var a=function(o){return wa($)(g)(n)(X(st)(r(o))(an))},u=Ma(t)(e);return ir({reflectType:function(){return`<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Ocarina.
  </p>

  @primer@
  @inOcarina@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(P()(P()(P()(P()(P()(rn()(P()(G)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inOcarina"}})({reflectSymbol:function(){return"inOcarina"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(D.value)(s1)({next:a(j_.value),primer:M(nT),inOcarina:M(eT),flavors:M(rT),ex0:M(j0(u)(r)(n)),ex1:M(Z0(u)(r)(n)),ex2:M(tT(u)(r)(n))})}}}};var m1=function(){return D.value}(),uT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = pure $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(m1)({ai0:M(ht(e)(t)(function(n){return bo(Fn)($t(xi)($t(xi)($t(xi)(p(nl)(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})(xn(Fn)(At(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(At(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(At(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(xn(Fn)(At(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return pt(n)([Ft(nt)(1)(function(){var u=function(o){return f(g)(tf()(Vc)(Hs()(Sr(ka)(o))(M_)))};return[qn(Va)(a.tink0)(u(.1)),qn(Va)(a.tink1)(u(.2)),qn(Va)(a.tink2)(u(.9)),qn(Va)(a.tink3)(u(1.8))]}())])}}))})}}};var d1=function(){return D.value}(),oT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = pure $ onOff $ dt (add n) apOn
            mtk i =
              case i \`mod\` 4 of
                0 -> tink0
                1 -> tink1
                2 -> tink2
                _ -> tink3
          0 .. 100 &lt;#&gt;
            \\i' -> do
              let i = toNumber i'
              playBuf (mtk i')
                (ooo (0.3 + 0.3 * (i * (1.005 \`pow\` i))))</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(d1)({ai0:M(ht(e)(t)(function(n){return bo(Fn)($t(xi)($t(xi)($t(xi)(p(nl)(function(a){return function(u){return function(o){return function(i){return{tink0:a,tink1:u,tink2:o,tink3:i}}}}})(xn(Fn)(At(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(At(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(xn(Fn)(At(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(xn(Fn)(At(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return pt(n)([Ft(nt)(1)(function(){var u=function(i){return f(g)(tf()(Vc)(Hs()(Sr(ka)(i))(M_)))},o=function(i){var _=Ya(Ku)(i)(4);return _===0?a.tink0:_===1?a.tink1:_===2?a.tink2:a.tink3};return Wr(hr)(un(0)(100))(function(i){var _=Ur(i);return qn(Va)(o(i))(u(.3+.3*(_*rc(1.005)(_))))})}())])}}))})}}};var y1=function(){return D.value}(),iT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<div>
  <pre><code>run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
        , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
        , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
        , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
        , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(y1)({ai0:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Oa(ar(br)(a)(it()))(function(u){return function(o){return Ft(nt)(.8)([Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:400,q:1})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:880,q:5})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:1200,q:10})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:2e3,q:20})([u]),Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:3e3,q:30})([u])])}})])}}))})}}};var k1=function(){return D.value}(),cT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \\i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(k1)({ai0:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Oa(ar(br)(a)(it()))(function(u){return function(o){return Ft(nt)(.8)(Wr(hr)(un(0)(40))(qa(In)(Ur)(function(i){return Dn(nn(bt(dt()(q(q(Dt)(mn)()()()({reflectSymbol:function(){return"q"}}))(en)()()()({reflectSymbol:function(){return"frequency"}})))(mt()())))({frequency:200+i*150,q:30})([u])})))}})])}}))})}}};var C1=function(){return D.value}(),fT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(C1)({ai0:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return pt(n)([au(function(u){return Ft(nt)(1)([qn(Va)(a)(it()),Do(Je)(.1)([Ft(nt)(.6)([u])])])})])}}))})}}};var E1=function(){return D.value}(),T1=function(t){return function(r){return f(t)(yn(r)(Sn)({p:[1,1,0],o:0,d:10}))}},S1=function(t){return function(r){return f(t)(yn(r)(Sn)({p:[1,1,0],o:0,d:8}))}},dl=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return Do(t)(n)([Ft(r)(a)([Kf(e)(u)(o)])])}}}}}}},lT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(E1)({txt:M(Zr(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = pure
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = pure
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 10.0 }

scene buf = run2_
  [ fan1 (playBuf buf bangOn) \\b _ -> mix $ fix
      \\g0 -> gain_ 1.0
        [ b
        , dgh 0.15 0.7 1500.0
            [ fix
                \\g1 -> gain 1.0 fade1
                  [ dgh 0.4 0.5 2500.0
                      [ g0, g1 ]
                  ]
            ]
        , dgh 0.29 0.85 2000.0
            [ fix
                \\g1 -> gain_ 1.0
                  [ dgh 0.6 0.6 3500.0
                      [ g0
                      , ( fix
                            \\g2 -> gain 1.0 fade0
                              [ dgh 0.75 0.6 4000.0
                                  [ g1, g2 ]
                              , dgh 0.75 0.55 3000.0 [ b ]
                              ]
                        )
                      ]
                  ]
            ]
        ]
  ]`)),ai0:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return pt(n)([Oa(qn(Va)(a)(it()))(function(u){return function(o){return au(function(i){return Ft(nt)(1)([u,dl(Je)(nt)(nu)(.15)(.7)(1500)([au(function(_){return Le(nt)(1)(T1(g)())([dl(Je)(nt)(nu)(.4)(.5)(2500)([i,_])])})]),dl(Je)(nt)(nu)(.29)(.85)(2e3)([au(function(_){return Ft(nt)(1)([dl(Je)(nt)(nu)(.6)(.6)(3500)([i,au(function(m){return Le(nt)(1)(S1(g)())([dl(Je)(nt)(nu)(.75)(.6)(4e3)([_,m]),dl(Je)(nt)(nu)(.75)(.55)(3e3)([u])])})])])})])])})}})])}}))})}}};var F1=function(){return D.value}(),_T=function(t){return function(r){return function(e){return function(n){var a=function(u){return wa($)(g)(n)(X(st)(r(u))(an))};return ir({reflectType:function(){return`<section>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how ocarina handles all three cases:</p>
    <ul>
      <li><span style="font-weight:800px;"><code>Array</code></span> is the data structure we'll use to send many audio units into one.</li>
      <li><span style="font-weight:800px;"><code>fan</code></span> is a function that we'll use to "fan" one audio node out to many.</li>
      <li><span style="font-weight:800px;"><code>fix</code></span> is the function we'll use to make an audio unit an input into itself.</li>
    </ul>
    <h2>The setup</h2>
    <p>
      To illustrate how <code>Array</code>, <code>fan</code> and <code>fix</code> work, we're going to use a few new audio units.
    </p>
      <ul>
        <li><code>delay</code>: A delay node</li>
        <li><code>bandpass</code>: A bandpass filter, meaning a filter that lets a single frequency band pass through.</li>
        <li><code>playBuf</code> and <code>loopBuf</code>: Playback or looping playback of a buffer. We'll use some MP3 files from freesound.org.</li>
      </ul>
  </section>`}})({reflectType:function(){return"@"}})()()(rn()(G)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}))(D.value)(F1)({hwLink:a(Kc.value)})}}}};var $1=function(){return D.value}(),pT=function(t){return function(r){return function(e){return function(n){var a=function(o){return wa($)(g)(n)(X(st)(r(o))(an))},u=Ma(t)(e);return ir({reflectType:function(){return`<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Ocarina graph</h3>

  @intro@

  <h2>Arrays</h2>

  <p>To send several audio units through one, we use an <code>Array</code>.</p>

  @code0@

  <p>PureScript <code>Array</code>-s are extremely flexible and efficient, so go to town! For example, you can <code>map</code> (aka <code>&lt;#&gt;</code> when flipped) over a range of integers to create audio units, like in the example below.</p>

  @code1@

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes a vector of audio signals and fans it out to multiple processing chains. If you have a single signal, you can use <code>fan1</code>. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>A vector of nodes to fan out (or a single node in the case of <code>fan1</code>).</li>
    <li>A function that accepts a reference to this/these node(s) and returns a new node that may or may not contain the input.</li>
  </ul>

  <p>Let's see an example below that fans one <code>playBuf</code> to five bandpass filters.</p>

  @code2@

  <p>Just for kicks, let's jack it up to forty bandpass filters.</p>

  @code3@

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts itself as an argument and returns... itself \u{1F92F}. You can use <code>fix</code> to create feedback loops!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Ocarina doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section \u{1F3B8}</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(P()(P()(P()(P()(P()(P()(P()(rn()(G)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}))(D.value)($1)({intro:M(_T(t)(r)(e)(n)),next:a(G_.value),code0:M(uT(u)(r)(n)),code1:M(oT(u)(r)(n)),code2:M(iT(u)(r)(n)),code3:M(cT(u)(r)(n)),code4:M(fT(u)(r)(n)),code5:M(lT(u)(r)(n))})}}}};var sT=function(t){return function(r){return new F(z("code")(t)(N(r)))}},zb=sT(x(h));var vT=function(t){return function(r){return new F(z("pre")(t)(N(r)))}},Vb=vT(x(h));var I1=function(){return D.value}(),mT=function(t){return function(r){return function(e){return function(n){var a=X(st)(r(J_.value))(an),u=Ma(t)(e);return ir({reflectType:function(){return`<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Ocarina. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

  @code@
  @result@

  <h2>The <code>run</code> functions</h2>

  <p>The <code>run</code> family of functions run our audio and produces an unsubscribe function that we use to stop the audio. In this case, <code>run2_</code> does three extra things:
  <ul>
    <li>Wires up our session for two-channel audio. If the sources are mono, it will automatically scale them up to stereo.</li>
    <li>Automatically handles creation and destruction of audio contexts.</li>
    <li>Takes care of the subscription to the rendering engine.</li>
  </ul></p>

  <p>The <code>push</code> function comes from the <a href="https://github.com/mikesol/purescript-deku"><code>purescript-deku</code></a> framework and is used to register an unsubscribe effect. When we trigger the effect it in the <code>Just</code> branch of our pattern match, the audio turns off.</p>

  <h2>Audio units</h2>

  <p>The sound you hear when you play the example above is created with the statement <code>gain_ 0.15 [ sinOsc 440.0 bangOn ]</code>. The first function, <code>gain_</code>, creates a gain node with a volume of <code>0.15</code>. In WebAudio, gain ranges from <code>0.0</code> to <code>1.0</code> and can be converted to decibels using the following equation:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>In our case, a gain of <code>0.15</code> is roughly <code>-16.5 dB</code>.</p>

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Ocarina. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Ocarina graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(P()(rn()(P()(G)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(D.value)(I1)({code:M(Vb([zb([Zr(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:M(ht(n)(u)(function(o){return f(da)(void 0)})(function(o){return function(i){return pt(o)([Ft(nt)(.15)([Qc(cc)(440)(it())])])}})),next:wa($)(g)(n)(a)})}}}};var DT=Tf;var dT=function(){return function(t){return t}},bT=function(){return function(t){return t}};var Gb=function(){function t(){}return t.value=new t,t}();var yT={attr:function(t){return function(r){return b({key:"height",value:B(r)})}}};var Jb=function(){function t(){}return t.value=new t,t}();var AT={attr:function(t){return function(r){return b({key:"width",value:B(r)})}}};var jb=function(t){return function(r){return new F(z("canvas")(t)(N(r)))}};var Xb=function(){function t(){}return t.value=new t,t}();var Qb={attr:function(t){return function(r){return b({key:"@self@",value:at(r)})}}};function Cv(t){return function(){return t.getContext("2d")}}function ip(t){return function(r){return function(){t.fillStyle=r}}}function hv(t){return function(){t.beginPath()}}function Ev(t){return function(){t.fill()}}function Kb(t){return function(r){return function(){t.arc(r.x,r.y,r.radius,r.start,r.end,r.useCounterClockwise)}}}function Tv(t){return function(r){return function(){t.fillRect(r.x,r.y,r.width,r.height)}}}var Z1=function(){return 2*ec}(),bl=function(t){return{o:t.value0+.04,n:t.value1,t:Vo}};var tB=function(){return D.value}(),yl=function(t){return function(r){return function(e){return function(n){return f(t)(Ao(r)(Sn)({p:[e,n],o:0,d:16}))}}}},rB=function(t){return function(r){return f(t)(yn(r)(Sn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},eB=function(t){return function(r){return f(t)(yn(r)(Sn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var Sv=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(_){return function(m){return q_(t)(n)(a)([Le(r)(u)(o)([id(e)(i)(_)(m)])])}}}}}}}}}},kT=function(t){return function(r){return function(e){return function(n){return function(a){return function(u){return function(o){return function(i){return function(_){return function(m){return q_(t)(n)(a)([Le(r)(u)(o)([od(e)(i)(_)(m)])])}}}}}}}}}},nB=function(t){return function(r){return function(e){return function(n){return f(t)(ll(r)(Sn)({p:[e,n],o:0,d:16}))}}}},gT=400,Yb=Ur(gT),aB=function(){return Wt(Na)(gT)+"px"}(),CT=600,Zb=Ur(CT),uB=function(){return Wt(Na)(CT)+"px"}(),oB={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},hT=function(t){return function(r){return function(e){return ir({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(D.value)(tB)({ex1:M(Wu()(Yr({reflectSymbol:function(){return"canvas"}})()()(Yr({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(Yr({reflectSymbol:function(){return"loading"}})()()(Yr({reflectSymbol:function(){return"start"}})()()(Yr({reflectSymbol:function(){return"stop"}})()()(Pn)()())()())()())(Pn)()())()())()())(D.value)(function(n){return function(a){var u=O($)(f(g)(void 0))(a.startStop.start),o=function(i){return function(_){return function(m){var l=p(k)(function(s){return new Q(s.acTime,s.value)})(pl(i)(a.slider));return[Zs(Ys(bt(dt()(q(q(Dt)(Ks)()()()({reflectSymbol:function(){return"fftSize"}}))(Qs)()()()({reflectSymbol:function(){return"cb"}})))(mt()())))({cb:function(s){return function(){return se(new T(s))(m)(),se(L.value)(m)}},fftSize:Us.value})(f(ce)(Oa(qn(Va)(_)(O($)(it())(p(k)(function(){var s=aa()(we),v=Un(vn)(Pa(0)(.96)(100)(1.04));return function(c){return s(bl(v(c)))}}())(l))))(function(s){return function(v){return au(function(c){return Ft(nt)(1)([s,q_(ed(bt(dt()(q(q(Dt)(sh)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(zD)()()()({reflectSymbol:function(){return"delayTime"}})))(mt()())))({maxDelayTime:2.5,delayTime:1})(p(k)(function(){var d=ll()(we),rt=Un(vn)(Pa(0)(.5)(100)(2.45));return function(Z){return d(bl(rt(Z)))}}())(l))([Le(nt)(.4)(p(k)(function(){var d=yn()(we),rt=Un(vn)(Pa(0)(.6)(100)(.9));return function(Z){return d(bl(rt(Z)))}}())(l))([s])]),Sv(Je)(nt)(nu)(.15)(x(h))(.7)(x(h))(1500)(yl(g)()(1500)(3e3))([au(function(d){return Le(nt)(1)(rB(g)())([Sv(Je)(nt)(nu)(.4)(x(h))(.5)(x(h))(3e3)(yl(g)()(3e3)(100))([c,d])])})]),Sv(Je)(nt)(nu)(.29)(p(k)(function(){var d=ll()(we),rt=Un(vn)(Pa(0)(.1)(100)(.4));return function(Z){return d(bl(rt(Z)))}}())(l))(.85)(x(h))(2e3)(yl(g)()(2e3)(5e3))([au(function(d){return Ft(nt)(1)([Sv(Je)(nt)(nu)(.6)(p(k)(function(){var rt=ll()(we),Z=Un(vn)(Pa(0)(.8)(100)(.3));return function(Vt){return rt(bl(Z(Vt)))}}())(l))(.6)(x(h))(3500)(yl(g)()(3500)(100))([c,au(function(rt){return Le(nt)(1)(eB(g)())([kT(Je)(nt)(nd)(.75)(p(k)(function(){var Z=ll()(we),Vt=Un(vn)(Pa(0)(.9)(100)(.1));return function(zt){return Z(bl(Vt(zt)))}}())(l))(.6)(x(h))(4e3)(yl(g)()(4e3)(200))([d,rt]),kT(Je)(nt)(nd)(.75)(nB(g)()(.75)(.2))(.55)(x(h))(200)(yl(g)()(200)(4e3))([s])])})])])})])])})}})))]}}};return Lr([jb(O($)(on(Lt)(h)(f(g))([J(AT)(Jb.value)(uB),J(yT)(Gb.value)(aB),J(Ik)(Bt.value)("width: 100%;"),J(Qb)(Xb.value)(function(){var i=$e(ft)(jr)(function(_){return function(){var l=Cv(_)();return ip(l)("black")(),Tv(l)({width:Zb,height:Yb,x:0,y:0})(),void 0}});return function(_){return i(lD(_))}}())]))(p(k)(function(i){return J(Qb)(Xb.value)(function(){var _=$e(ft)(jr)(function(m){return function(){var s=Cv(m)();return ip(s)("black")(),Tv(s)({width:Zb,height:Yb,x:0,y:0})(),ip(s)("rgba(255,255,255,0.2)")(),Pu(i)(function(v){return function(){return hv(s)(),Kb(s)({end:Z1,radius:v.value1*40,start:0,x:v.value0.x*Zb,y:v.value0.y*Yb,useCounterClockwise:!1})(),Ev(s)()}})()}});return function(m){return _(lD(m))}}())})(a.canvas)))([]),go(on(Lt)(h)(f(g))([J(Qo)(ko.value)("range"),J(vc)(Mi.value)("0"),J(sc)(wi.value)("100"),J(Dc)(Ii.value)("1"),J(dc)(Ri.value)("50"),J(Pk)(Bt.value)("width: 100%;"),J(mc)(Pi.value)(Gr(function(){var i=$e(ft)(jr)(af(Yn)(qc)(n.slider)),_=Jn(ga)(Rf);return function(m){return i(_(Dl(m)))}}()))]))([]),dn(be(Lt)(h)([f(g)(J(Sf)(Bt.value)("width:100%; padding:1.0rem;")),on(Lt)(h)(p(k)(function(){var i=J(fe)(ue.value);return function(_){return i(Gr(E(_)))}}()))([V(k)(a.startStop.loading)(f(ft)(void 0)),Wr(k)(a.startStop.stop)(function(i){return X(st)(i)(X(st)(t(f(ft)(void 0)))(n.startStop.start(void 0)))}),Wr(k)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(i){return i.value0})(e)))(V(k)(u)(j(K))))(function(i){return function(){i(),n.startStop.loading(void 0)();var m=Nr(L.value)(),l=yo(ct(Fe)(na(xe))(function(s){return ct(Fe)(Gu(xe)(s))(function(v){return ct(Fe)(p(Si)(bT())(eE(Fn)(DT)(At(s))(dT()(oB))))(function(c){return ct(Fe)(le(xe)(p_(0)(5e4)))(function(d){var rt=ic(fD(fa(d_(c.pluck0))($f(Lm(Bm()(c))))))({newSeed:nc(d),size:4});return le(xe)(function(){var Vt=Mn(ro)(ft)(function(ut){return function(){var Cr=Uo(),Et=Uo();return{x:Cr,y:Et}}})(un(0)(127))(),zt=pt(s)(o(s)(rt)(m))(),Jr=Re(lc)(function(ut){return function(){var Cr=Vr(m)();return kn(ft)(jr)(Cr)(function(Et){return function(){var Hn=K_(Et)(),Be=p(R)(function(){var ya=Wl(Vt),ua=p(hr)(function(Ue){return function(Ju){return Ju/255}(Ue)});return function(Ue){return ya(ua(Ue))}}())(_v(lv)(Hn))();return n.canvas(Be)(),void 0}})()}})(),ie=X(st)(X(st)(X(st)(zt)(v))(bn(te)(s)))(Jr);return n.startStop.stop(ie)(),ie})})})})}))();return t(function(){return n.startStop.start(void 0)(),Jo(Fi(l))()})(),void 0}})])]))([tn(be(Lt)(h)([p(k)(E("Turn off"))(a.startStop.stop),p(k)(E("Turn on"))(u),p(k)(E("Loading..."))(a.startStop.loading)]))])])}}))})}}};var cB=function(){return D.value}(),ET=function(t){return function(r){return function(e){return function(n){var a=Ma(t)(e);return St({reflectType:function(){return`<div>
  <h1>Ocarina</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-ocarina">Ocarina</a>.</p>

  <p>Ocarina is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Ocarina tackles this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Ocarina that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-ocarina/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la ocarina</a>!</p>
</div>`}})()()(P()(rn()(G)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}}))(cB)({next:wa($)(g)(n)(X(st)(r(Kc.value))(an)),ex:M(hT(a)(r)(n))})}}}};var lB=function(){return D.value}(),TT=function(t){return function(r){return function(e){return function(n){return St({reflectType:function(){return`<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Ocarina allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in ocarina looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>`}})()()(rn()(G)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(lB)({next:f(g)(J(fe)(ue.value)(Gr(E(X(st)(r(ev.value))(an)))))})}}}};var pB=function(){return D.value}(),ST=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2>Cancel</h2>
  <p>The <code>AudioCancel</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues"><code>cancelScheduledValues</code></a> function and cancels whatever effects you programmed in the future. In the example below, we execute the following sequence:</p>
  <ol>
    <li>Play an audio file</li>
    <li>Send an event at 1.0 seconds to schedule an evenlope to modulate the audio rate starting at 1.5 seconds.</li>
    <li>Cancel the envelope at 3.0 seconds, but schedule the cancelation to take effect at 4.0 seconds.</li>
  </ol>
  <pre><code>~txt~</code></pre>
  ~cancel~
  </section>
`}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(pB)({txt:M(Zr(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ pure
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (pure (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]`)),cancel:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Ft(nt)(1)([ar(br)(a)(be(Lt)(h)([it(),Nu(1e3)(f(g)(aa()(Sn)({p:ei(ri)(V(hr)(un(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Nu(3e3)(f(g)(aa()(ZC)({o:3.5})))]))])])}}))})}}};var vB=function(){return D.value}(),xT=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))(vB)({txt:M(Zr(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ pure
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]`)),envelope:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Ft(nt)(1)([ar(br)(a)(be(Lt)(h)([it(),Nu(1e3)(f(g)(aa()(Sn)({p:ei(ri)(V(hr)(un(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}}))})}}};var DB=function(){return D.value}(),FT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2>AudioNumeric</h2>
  <p><code>AudioNumeric</code> encompasses the following three functions from the Web Audio API:</p>

  <ul>
    <li><code>linearRampToValueAtTime</code> via the <code>_linear</code> transition.</li>
    <li><code>exponentialRampToValueAtTime</code> via the <code>_exponential</code> transition.</li>
    <li><code>setValueAtTime</code> via the <code>_step</code> transition.</li>
  </ul>

  <p>Let's explore all of them in the example below.</p>

  <blockquote>Pro tip: When using <code>AudioNumeric</code>, consider starting with a <code>_step</code> transition. Otherwise, the transition may be abrupt and unpleasant!</blockquote>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            pure
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            pure
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}}))(D.value)(DB)({numericEx:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Ft(nt)(1)([ar(br)(a)(Ko($)(it())(function(){return Ko($)(Nu(1e3)(Ko($)(f(g)(aa()(we)({n:1,o:1,t:BD})))(function(){return f(g)(aa()(we)({n:1.3,o:2,t:Vo}))})))(function(){return Nu(2500)(Ko($)(f(g)(aa()(we)({n:1,o:2.5,t:BD})))(function(){return f(g)(aa()(we)({n:.7,o:3.5,t:YC}))}))})}))])])}}))})}}};var bB=function(){return D.value}(),OT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1500
            $ pure
            $ playbackRate
            $ AudioSudden { n: 1.4 }
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(P()(G)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}}))(D.value)(bB)({suddenEx:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([Ft(nt)(1)([ar(br)(a)(be(Lt)(h)([it(),Nu(1500)(f(g)(aa()(QC)({n:1.4})))]))])])}}))})}}};var AB=function(){return D.value}(),$T=function(t){return function(r){return function(e){return St({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      pure
        $ playbackRate
        $ c1
            ( gain_ 1.0
                [ constant 1.0 bangOn
                , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                ]
            )
      )
  ]
</code></pre>

  ~unitEx~
  </section>
`}})()()(P()(G)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}}))(AB)({unitEx:M(ht(e)(t)(function(n){return At(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return pt(n)([ar(br)(a)(be(Lt)(h)([it(),f(g)(aa()(JC(ki)(ki))(GC(Ft(nt)(1)([rv(Gs)(1)(it()),Ft(nt)(.2)([Yf(js)(100)([U_(qf)(50)(it())])])]))))]))])}}))})}}};var gB=function(){return D.value}(),wT=function(t){return function(r){return function(e){return function(n){var a=X(st)(r(X_.value))(an),u=Ma(t)(e);return St({reflectType:function(){return`<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how ocarina exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>`}})()()(P()(P()(rn()(P()(P()(P()(G)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(gB)({sudden:M(OT(u)(r)(n)),numeric:M(FT(u)(r)(n)),envelope:M(xT(u)(r)(n)),cancel:M(ST(u)(r)(n)),unit:M($T(u)(r)(n)),next:wa($)(g)(n)(a)})}}}};var hB=function(){return D.value}(),MT=function(t){return function(r){return function(e){return function(n){return St({reflectType:function(){return`<div>
  <h1>Tumult</h1>

  <h2>The unknown unknowns of web audio</h2>
  <p>
    As we saw in the last section on subgraphs, sometimes, you simply don't know how your audio will evolve. For example, if we are building a live coding environment and create a space where someone can basically do <i>anything</i>, working with anonymous audio nodes will not cut it. For example, imagine that they have two relatively unconnected parts of the audio graph and want to start creating cross-connections. With subgraphs, we can't pick apart the graph and say "take node X and connect it to node Y" as nodes don't have names.
  </p>

  <p>
    Tumult solves this problem. It is an entirely separate rendering engine from the one we've seen before that works via <i>diffing</i> two audio graphs and adjusting the current graph based on the diff. The biggest difference is that tumult <i>requires</i> you to give a name to each audio unit. Otherwise, the engine won't know what node corresponds to what name.
  </p>

  <p>
    For you veteran coders and audio-ers, you may bristle at the word <i>diff</i>. After all, diffing is expensive, and we are working in resource constrained environments where timing is of the essence. Fear not! Tumult uses PureScript <code>Set</code>-s to do blazingly fast diffs. That said, tumult is the slowest part of ocarina (meaning it is just blazingly fast instead of obscenely and indecently fast), so only use it where you can afford a performmance hit.
  </p>

  <h2>Hello tumult</h2>

  <h2>Stability in tumult</h2>

  <p>While tumult will run a diff every time its event is triggered, if you don't tear down a node, its nodes will merrily putter along with whatever events you send them. This means that you can trigger tumult events <i>only</i> when you need to change the shape of the graph.</p>

  <h2>Feeling lucky</h2>

  <p>So far, we have only seen the <code>DiffMe</code> instruction sent to tumult. There is another instruction you can send called <code>FeelingLucky</code>. <code>FeelingLucky</code> will attempt to perform its instruction and be a no-op if it is incoherent with respect to the graph. Careful with this, though! <code>DiffMe</code> guarantees that whatever you send to tumult will be what's rendered from your loudspeaker. Too many <code>FeelingLucky</code>-s, on the other hand, can lead to bugs where you're not quite sure anymore <i>what</i> is playing. That said, <code>FeelingLucky</code> is grotesquely fast, especially for larger graphs. So if it makes sense to use it, use it!</p>

  <h2>Next steps</h2>
  <p>In this section, we learned how to use tumult to create truly dynamic audio graphs that allow you to fine-tune the speed-versus-dynamism tradeoff in various ways.</p>
  <p>In the next section, we'll look at how to create audio graphs via an <a ~next~ style="cursor:pointer;">imperative API that more closely resembles Web Audio while providing additional type-safety benefits</a>.</p>
</div>`}})()()(rn()(G)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(hB)({next:f(g)(J(fe)(ue.value)(Gr(E(X(st)(r(nv.value))(an)))))})}}}};var TB=function(){return D.value}(),PT=function(t){return function(r){return function(e){return function(n){return St({reflectType:function(){return`<div>
  <h1>Audio Worklets</h1>

  <h2>Supercharging our audio experiences</h2>

  <p>The Web Audio lets you do basically anything, but when anything isn't enough, there are custom Audio Worklets. </p>

  <p>Custom audio worklets come in a few shapes an sizes:</p>

  <ul>
    <li>You can <a href="https://developer.chrome.com/blog/audio-worklet/">write your own in JavaScript</a>. While this is ok for small PoCs, it will get really slow really quick.</li>
    <li>You can compile your favorite C/C++/Rust audio processors to <a href="https://developer.chrome.com/blog/audio-worklet-design-pattern/#using-audio-worklet-with-webassembly">web assembly</a>. The helpful folks at Google have created many examples that do exactly this, and you can pilfer them for fun and profit.</li>
    <li>My go-to solution is Faust. <a href="https://faust.grame.fr/">Faust</a> is the greatest audio project on Earth, and let's you build very performant audio processors with an expressive and concise syntax. Faust's <a href="https://webaudioconf.com/posts/2017_EA_60/">Web Audio integration</a> is a one-liner from the command line.</li>
  </ul>

  <p>In this example, we'll use Faust to create a three custom audio units and wire them together using Ocarina. The units are:</p>

  <ul>
    <li>A wicked sawtooth synth.</li>
    <li>A kickin' comb filter.</li>
    <li>Freeverb, the free-software-world's favorite reverb!</li>
  </ul>

  <h2>Faust</h2>

  <p>Here are our three faust examples:</p>

  <h3>Sawtooth</h3>

  <h3>Comb filter</h3>

  <h3>Freeverb</h3>

  <h3>Compilation</h3>

  We can compile each example using the following commands. Each command creates a self-contained JavaScript file, so all we need to do is link to it in ocarina-land.

  <pre><code>placeholder</code></pre>

  <h2>Ocarina</h2>

  <p>Ocarina provides a type-safe interface for declaring the API of Audio Worklets. While it's your job to make sure the API is in fact the actual API of the worklet, assuming this is correct, ocarina will enforce it for you. Let's see how in the following example. Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h3>The API contract</h3>

  <p>The <code>InitializeAudioWorklet</code> API contract is defined like so:</p>

  <pre><code>InitializeAudioWorkletNode
  { name :: Proxy name
  , numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  }</code></pre>

  <p>And here is how the APIs parameters are used:</p>

  <table>
    <tr>
      <th>Parameter</th>
      <th>Description</th>
    </tr>
    <tr>
      <td><code>name</code></td>
      <td>The name of the worklet as defined in the JS output. When in doubt, check the JS produced by faust!</td>
    </tr>
    <tr>
      <td><code>numerOfInputs</code></td>
      <td>How many inputs does the node take? Note that this is <i>not</i> the number of channels, but rather the number of distinct inputs, like in a merger node.</td>
    </tr>
    <tr>
      <td><code>numerOfOutputs</code></td>
      <td>How many outputs does the node produce? Note that this is <i>not</i> the number of channels, but rather the number of distinct outputs, like in a splitter node.</td>
    </tr>
    <tr>
      <td><code>outputChannelCount</code></td>
      <td>How many channels are in each output? This is defined using a tuple of type-level numbers.</td>
    </tr>
    <tr>
      <td><code>parameterData</code></td>
      <td>Initial parameters for the audio node. This will also define what parameters can be sent to it via events (the record gets flipped to a <a href="https://github.com/natefaubion/purescript-variant"><code>Variant</code></a> when it is an event).</td>
    </tr>
  </table>

  <h2>Next steps</h2>
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via ocarina. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by ocarina. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there \u{1F605}</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a ocarina graph</a>.</p>
</div>`}})()()(rn()(G)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(TB)({next:f(g)(J(fe)(ue.value)(Gr(E(X(st)(r(Yc.value))(an)))))})}}}};var xB=function(){return D.value}(),IT=function(t){return function(r){return function(e){return function(n){return ir({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>`}})({reflectType:function(){return"~"}})()()(G)(D.value)(xB)({})}}}};var OB=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)

import FRP.Event.VBus (V, vbus)
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (Channel(..), dyn, bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)

type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Unit)

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
    :: String

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = pure unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0
              [ dyn $ map
                  ( \\i ->
                      OneOf.do
                        pure $ sound $ playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                        delay 5000 $ pure $ silence
                  )
                  sl
              ]
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( O.oneOfMap pure O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb (const (push.slider unit))
                )
                []
            ]
        , D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                startE $> (music >>= push.startStop.stop)
                event.startStop.stop <#>
                  (_ *> push.startStop.start unit)
            )
            [ text OneOf.do
                startE $> "Turn on"
                event.startStop.stop $> "Turn off"
            ]
        ]
`,$B=lu(function(t){return Sa(function(r){return Re(t)(function(e){return function(){var a=Uo();return r(e(a))()}})})}),wB=function(){return D.value}(),MB="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",RT=function(t){return function(r){return function(e){return ir({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(P()(P()(G)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(D.value)(wB)({txt:M(Zr(OB)),ex1:M(Wu()(Yr({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(Yr({reflectSymbol:function(){return"loading"}})()()(Yr({reflectSymbol:function(){return"start"}})()()(Yr({reflectSymbol:function(){return"stop"}})()()(Pn)()())()())()())(Pn)()())()())(D.value)(function(n){return function(a){var u=O($)(f(g)(void 0))(a.startStop.start),o=$i(k)(Q.create)($B)(Ru(Pt)(function(_){return function(m){return m+1|0}})(a.slider)(0)),i=function(_){return[Ft(nt)(1)([jp(p(k)(function(m){return be(Lt)(h)([f(g)(zC(qn(L_(bt(dt()(q(q(Dt)(oh)()()()({reflectSymbol:function(){return"playbackRate"}}))(I_)()()()({reflectSymbol:function(){return"buffer"}})))(mt()())))({buffer:_,playbackRate:.7+Ua(m)*2})(it()))),Nu(5e3)(f(g)(VC))])})(o))])]};return Lr([Lr([Zr("Slide me!"),go(on(Lt)(h)(f(g))([J(Qo)(ko.value)("range"),J(vc)(Mi.value)("0"),J(sc)(wi.value)("100"),J(Dc)(Ii.value)("1"),J(dc)(Ri.value)("50"),J(mc)(Pi.value)(Gr(E(n.slider(void 0))))]))([])]),dn(on(Lt)(h)(p(k)(function(){var _=J(fe)(ue.value);return function(m){return _(Gr(E(m)))}}()))([V(k)(a.startStop.loading)(f(ft)(void 0)),Wr(k)(a.startStop.stop)(function(_){return X(st)(_)(X(st)(t(f(ft)(void 0)))(n.startStop.start(void 0)))}),Wr(k)(En(Pt)(O($)(f(g)(f(ft)(void 0)))(p(k)(function(_){return _.value0})(e)))(V(k)(u)(j(K))))(function(_){return function(){_(),n.startStop.loading(void 0)();var l=yo(ct(Fe)(na(xe))(function(s){return ct(Fe)(Gu(xe)(s))(function(v){return ct(Fe)(At(s)(MB))(function(c){return le(xe)(function(){var rt=fv(i(c))(),Z=X(st)(X(st)(rt)(v))(bn(te)(s));return n.startStop.stop(Z)(),Z})})})}))();return t(function(){return n.startStop.start(void 0)(),Jo(Fi(l))()})(),void 0}})]))([tn(be(Lt)(h)([p(k)(E("Turn off"))(a.startStop.stop),p(k)(E("Turn on"))(u)]))])])}}))})}}};var IB=function(){return D.value}(),NT=function(t){return function(r){return function(e){return function(n){var a=Ma(t)(e);return St({reflectType:function(){return`<div>
  <h1>Subgraphs</h1>

  <h2>Making audio even more dynamic</h2>
  <p>
    When we're creating video games or other types of interactive work, it's rare that we'll be able to anticipate the exact web audio graph we'll need for an entire session. As an example, imagine that in a video game a certain sound effects accompany various characters, and those characters come in and out based on your progress through the game. One way to solve this would be to anticipate the maximum number of characters that are present in a game and do a round-robin assignment of nodes in the audio graph as characters enter and leave your game. But sometimes that's not ergonomic, and in certain cases its downright inefficient. Another downside is that it does not allow for specialization of the web audio graph based on new data, like for example a character to play a custom sound once you've earned a certain badge.
  </p>

  <p>
    Subgraphs fix this problem. They provide a concise mechansim to dynamically insert audio graphs based on events.
  </p>

  ~suby~

  <h2>Go forth and be brilliant!</h2>
  <p>Thus ends the first version of the ocarina documentation. Applause is always welcome ~appl~! Alas, some features remain undocumented, like audio worklets and an imperative API. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Ocarina!</p>
</div>`}})()()(P()(P()(G)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}}))(IB)({appl:M(cv("\u{1F44F}")(n)(a)(function(u){return At(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(o){return pt(u)([Ft(nt)(1)([ar(br)(o)(it())])])}})),suby:M(RT(a)(r)(n))})}}}};var iBt=function(t){return t},cBt={Coercible0:function(){}},NB=function(){var t=function(r){var e=function(n){if(n instanceof V_)return Lr(f(ce)(Ve(ET(r.setCancellation)(r.setPage))));if(n instanceof Kc)return Lr(f(ce)(Ve(mT(r.setCancellation)(r.setPage))));if(n instanceof J_)return Lr(f(ce)(Ve(pT(r.setCancellation)(r.setPage))));if(n instanceof G_)return Lr(f(ce)(Ve(l0(r.setCancellation)(r.setPage))));if(n instanceof ev)return Lr(f(ce)(Ve(PT(r.setCancellation)(r.setPage))));if(n instanceof Yc)return Lr(f(ce)(Ve(aT(r.setCancellation)(r.setPage))));if(n instanceof j_)return Lr(f(ce)(Ve(wT(r.setCancellation)(r.setPage))));if(n instanceof X_)return Lr(f(ce)(Ve(G0(r.setCancellation)(r.setPage))));if(n instanceof nv)return Lr(f(ce)(Ve(IT(r.setCancellation)(r.setPage))));if(n instanceof mE)return Lr(f(ce)(Ve(TT(r.setCancellation)(r.setPage))));if(n instanceof Q_)return Lr(f(ce)(Ve(NT(r.setCancellation)(r.setPage))));if(n instanceof DE)return Lr(f(ce)(Ve(MT(r.setCancellation)(r.setPage))));throw new Error("Failed pattern match at Ocarina.Example.Docs (line 145, column 5 - line 145, column 76): "+[n.constructor.name])};return e(r.page)};return Bk(Lk(new al(V_.value)))(function(r){var e=Ru(Pt)(function(n){if(n instanceof al)return function(a){return{prevPage:new T(a.curPage),curPage:n.value0,cancel:a.cancel,pageChange:!0}};if(n instanceof _d)return function(a){return{cancel:n.value0,pageChange:!1,curPage:a.curPage,prevPage:a.prevPage}};throw new Error("Failed pattern match at Ocarina.Example.Docs (line 57, column 15 - line 59, column 83): "+[n.constructor.name])})(r.value1)({prevPage:L.value,curPage:V_.value,cancel:f(ft)(void 0),pageChange:!0});return Lr([Lr(p(hr)(function(n){return Gm([Vm(O($)(on(Lt)(h)(f(g))([J(fe)(ue.value)(Gr(E(r.value0(new al(n.value0))))),J(Rk)(Bt.value)("cursor:pointer;")]))(p(k)(function(a){return J(fe)(ue.value)(Gr(E(function(){return a.cancel(),r.value0(new al(n.value0))()})))})(Jl(bu)(function(){var a=ru(Ba);return function(u){return a(function(o){return o.pageChange}(u))}}())(e))))([Zr(n.value1.value0)]),Ff(f(g)(J(ts)(Bt.value)(function(){return n.value1.value1?"":"display:none;"}())))([Zr(" | ")])])})([new Q(V_.value,new Q("Home",!0)),new Q(Kc.value,new Q("Hello world",!0)),new Q(J_.value,new Q("Array, fan, and fix",!0)),new Q(G_.value,new Q("Audio units",!0)),new Q(Yc.value,new Q("Events",!0)),new Q(j_.value,new Q("Parameters",!0)),new Q(X_.value,new Q("State",!0)),new Q(Q_.value,new Q("Subgraphs",!1))])),$k(dr)(function(n){return t({page:n.curPage,setPage:function(a){return r.value0(al.create(a))},setCancellation:function(a){return r.value0(_d.create(a))}})})(Jl(bu)(function(n){return n.pageChange})(e))])})}(),fBt=function(t){return{page:t,setPage:vr(mp(Tc(Ol))),setCancellation:vr(mp(Tc(Ol)))}},lBt=Vg(NB);export{iBt as TopLevelSg,lBt as main,cBt as newtypeTopLevelSg_,fBt as p2tl,NB as scene};
