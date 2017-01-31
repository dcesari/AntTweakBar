Name:		AntTweakBar
Version:	1.16
Release:	1%{?dist}
Summary:	GUI library for videogame property editing UIs
Group:		Applications/Internet
License:	zlib
URL:		http://anttweakbar.sourceforge.net/doc/
Source0:	anttweakbar-%{version}.tar.gz
BuildRoot:  %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildRequires: mesa-libGL-devel
BuildRequires: mesa-libGLU-devel
BuildRequires: freeglut-devel
BuildRequires: libX11-devel
BuildRequires: xorg-x11-proto-devel

# DOS mode file in upstream source makes this necessary for any patches
%global _default_patch_fuzz 2

# autoconf lowercases
%define acname anttweakbar

%description
Library for easily creating and using tweakable properties in an OpenGL
or SDL application, designed primarily for professional game developers.

%package devel
Summary: Header files for AntTweakBar
Group: Development/Libraries
Requires: %{name} = %{version}-%{release}

%description devel
Header files for developing applications making use of AntTweakBar.

%prep
%setup -q -n %{acname}-%{version}

# remove BOM from license file in place
sed -i '1 s/^\xef\xbb\xbf//' License.txt

# fix DOS newlines in examples
for file in examples/TwAdvanced1.cpp examples/TwSimpleGLUT.c \
    examples/TwSimpleSDL.c examples/TwString.cpp; do \
    sed "s|\r||g" $file > $file.tmp; touch -r $file $file.tmp; mv $file.tmp $file
done

%build
%configure --enable-fortran --disable-examples FCFLAGS="%{optflags} -I%{_fmoddir}/GL"
make

%install
rm -rf "%{buildroot}"
make DESTDIR=%{buildroot} install
mkdir -p %{buildroot}%{_fmoddir}
mv %{buildroot}%{_includedir}/*.mod %{buildroot}%{_fmoddir}

%clean
rm -rf "%{buildroot}"

%files
%defattr(-,root,root,-)
%doc %{_docdir}/%{acname}/License.txt
%doc %{_docdir}/%{acname}/Readme.txt
%{_libdir}/libAntTweakBar.so.*

%files devel
%defattr(-,root,root,-)
%{_includedir}/*
%{_fmoddir}/*
%{_libdir}/libAntTweakBar.so
%{_libdir}/libAntTweakBar.la
%{_libdir}/libAntTweakBar.a
%doc %{_docdir}/%{acname}/Readme_fortran.txt
%doc %{_docdir}/%{acname}/*.c
%doc %{_docdir}/%{acname}/*.cpp
%doc %{_docdir}/%{acname}/*.f90

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%changelog
* Tue Jul 08 2014 David Brown <david.brown@pnnl.gov> - 1.16-1
- New upstream version

* Fri Jun 06 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.14-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Fri Aug 02 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.14-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Wed Feb 13 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.14-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Thu Aug 16 2012 David Brown <david.brown@pnnl.gov> - 1.14-5
- Upstream put out new source tarball
- Fixed the shared library symlinks as well
- Fixed more dos new lines

* Wed Jul 18 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.14-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Thu Jan 12 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.14-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Sun Jun 12 2011 Sean Middleditch <sean@middleditch.us> - 1.14-2
- Use the RPM_OPT_FLAGS in building on newer Fedora setups.

* Sat Jun 11 2011 Sean Middleditch <sean@middleditch.us> - 1.14-1
- Updated to upsteam 1.14 release.
- Dropped upstream patches.

* Mon Feb 07 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.13-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Fri Oct 23 2009 Sean Middleditch <sean@middleditch.us> 1.13-5
- Correct accidental macro expansion in changelog text.

* Fri Oct 23 2009 Sean Middleditch <sean@middleditch.us> 1.13-4
- Use %%global instead of %%define.
- Note that patches have been sent to upstream.

* Fri Oct 23 2009 Sean Middleditch <sean@middleditch.us> 1.13-3
- Preserve timestamp on License.txt.
- Renamed patches to comply with naming policy.
- Included Linux-relevant examples in documentation for -devel package.

* Thu Oct 22 2009 Sean Middleditch <sean@middleditch.us> 1.13-2
- Fix shared library soname and ld links.

* Wed Oct 21 2009 Sean Middleditch <sean@middleditch.us> 1.13-1
- Initial RPM release.
