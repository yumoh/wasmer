(function() {var implementors = {};
implementors["wasmer_types"] = [{"text":"impl Send for Features","synthetic":true,"types":[]},{"text":"impl Send for CustomSectionIndex","synthetic":true,"types":[]},{"text":"impl Send for DataIndex","synthetic":true,"types":[]},{"text":"impl Send for ElemIndex","synthetic":true,"types":[]},{"text":"impl Send for FunctionIndex","synthetic":true,"types":[]},{"text":"impl Send for GlobalIndex","synthetic":true,"types":[]},{"text":"impl Send for LocalFunctionIndex","synthetic":true,"types":[]},{"text":"impl Send for LocalGlobalIndex","synthetic":true,"types":[]},{"text":"impl Send for LocalMemoryIndex","synthetic":true,"types":[]},{"text":"impl Send for LocalTableIndex","synthetic":true,"types":[]},{"text":"impl Send for MemoryIndex","synthetic":true,"types":[]},{"text":"impl Send for SignatureIndex","synthetic":true,"types":[]},{"text":"impl Send for TableIndex","synthetic":true,"types":[]},{"text":"impl&lt;'data&gt; Send for DataInitializer&lt;'data&gt;","synthetic":true,"types":[]},{"text":"impl Send for DataInitializerLocation","synthetic":true,"types":[]},{"text":"impl Send for OwnedDataInitializer","synthetic":true,"types":[]},{"text":"impl Send for TableInitializer","synthetic":true,"types":[]},{"text":"impl Send for Atomically","synthetic":true,"types":[]},{"text":"impl&lt;'a, T, A&nbsp;=&nbsp;NonAtomically&gt; !Send for MemoryView&lt;'a, T, A&gt;","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; !Send for HostRef&lt;T&gt;","synthetic":true,"types":[]},{"text":"impl Send for Bytes","synthetic":true,"types":[]},{"text":"impl Send for PageCountOutOfRange","synthetic":true,"types":[]},{"text":"impl Send for Pages","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; Send for ExportType&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Send for FunctionType","synthetic":true,"types":[]},{"text":"impl Send for GlobalType","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; Send for ImportType&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Send for MemoryType","synthetic":true,"types":[]},{"text":"impl Send for TableType","synthetic":true,"types":[]},{"text":"impl Send for V128","synthetic":true,"types":[]},{"text":"impl Send for ExportIndex","synthetic":true,"types":[]},{"text":"impl Send for ImportIndex","synthetic":true,"types":[]},{"text":"impl !Send for ExternRef","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; !Send for Value&lt;T&gt;","synthetic":true,"types":[]},{"text":"impl Send for ExternType","synthetic":true,"types":[]},{"text":"impl Send for GlobalInit","synthetic":true,"types":[]},{"text":"impl Send for Mutability","synthetic":true,"types":[]},{"text":"impl Send for Type","synthetic":true,"types":[]},{"text":"impl Send for NonAtomically","synthetic":true,"types":[]},{"text":"impl !Send for InternalRef","synthetic":true,"types":[]},{"text":"impl !Send for AnyAndHostInfo","synthetic":true,"types":[]},{"text":"impl !Send for OtherRef","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; !Send for ContentBox&lt;T&gt;","synthetic":true,"types":[]},{"text":"impl&lt;K, V&gt; Send for BoxedSlice&lt;K, V&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;K: Send,<br>&nbsp;&nbsp;&nbsp;&nbsp;V: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;'a, K, V&gt; Send for Iter&lt;'a, K, V&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;K: Send,<br>&nbsp;&nbsp;&nbsp;&nbsp;V: Sync,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;'a, K, V&gt; Send for IterMut&lt;'a, K, V&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;K: Send,<br>&nbsp;&nbsp;&nbsp;&nbsp;V: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;K&gt; Send for Keys&lt;K&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;K: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;K, V&gt; Send for PrimaryMap&lt;K, V&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;K: Send,<br>&nbsp;&nbsp;&nbsp;&nbsp;V: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;K, V&gt; Send for SecondaryMap&lt;K, V&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;K: Send,<br>&nbsp;&nbsp;&nbsp;&nbsp;V: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; Send for PackedOption&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl&lt;K, V&gt; Send for IntoIter&lt;K, V&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;K: Send,<br>&nbsp;&nbsp;&nbsp;&nbsp;V: Send,&nbsp;</span>","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()