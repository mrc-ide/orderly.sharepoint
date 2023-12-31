% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location.R
\name{orderly_location_sharepoint}
\alias{orderly_location_sharepoint}
\title{Orderly Location for Sharepoint}
\description{
A driver to put orderly/orderly2 archives on
Sharepoint. Individual methods are not documented here as this
just needs to satisfy the interface required by orderly/orderly2
and is not for direct user use. See
\link[orderly2:orderly_location_add]{orderly2::orderly_location_add} for details.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-orderly_location_sharepoint-new}{\code{orderly_location_sharepoint$new()}}
\item \href{#method-orderly_location_sharepoint-list}{\code{orderly_location_sharepoint$list()}}
\item \href{#method-orderly_location_sharepoint-metadata}{\code{orderly_location_sharepoint$metadata()}}
\item \href{#method-orderly_location_sharepoint-fetch_file}{\code{orderly_location_sharepoint$fetch_file()}}
\item \href{#method-orderly_location_sharepoint-list_unknown_packets}{\code{orderly_location_sharepoint$list_unknown_packets()}}
\item \href{#method-orderly_location_sharepoint-list_unknown_files}{\code{orderly_location_sharepoint$list_unknown_files()}}
\item \href{#method-orderly_location_sharepoint-push_file}{\code{orderly_location_sharepoint$push_file()}}
\item \href{#method-orderly_location_sharepoint-push_metadata}{\code{orderly_location_sharepoint$push_metadata()}}
\item \href{#method-orderly_location_sharepoint-clone}{\code{orderly_location_sharepoint$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-new"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$new(url, site, path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{url}}{The base url of your Office365/Sharepoint site, such
as \verb{https://myorg.sharepoint.com} (the \verb{https://} prefix is required)}

\item{\code{site}}{Your site name on Sharepoint}

\item{\code{path}}{The path within your site name where documents will
be stored (you may need \verb{Shared Documents} even if Sharepoint
makes it look like \code{Documents}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-list"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-list}{}}}
\subsection{Method \code{list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$list()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-metadata"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-metadata}{}}}
\subsection{Method \code{metadata()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$metadata(packet_ids)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-fetch_file"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-fetch_file}{}}}
\subsection{Method \code{fetch_file()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$fetch_file(hash, dest)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-list_unknown_packets"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-list_unknown_packets}{}}}
\subsection{Method \code{list_unknown_packets()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$list_unknown_packets(ids)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-list_unknown_files"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-list_unknown_files}{}}}
\subsection{Method \code{list_unknown_files()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$list_unknown_files(hashes)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-push_file"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-push_file}{}}}
\subsection{Method \code{push_file()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$push_file(src, hash)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-push_metadata"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-push_metadata}{}}}
\subsection{Method \code{push_metadata()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$push_metadata(packet_id, root)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-orderly_location_sharepoint-clone"></a>}}
\if{latex}{\out{\hypertarget{method-orderly_location_sharepoint-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{orderly_location_sharepoint$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
