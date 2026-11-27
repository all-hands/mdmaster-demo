(function () {
  "use strict";

  var WIDE = "(min-width: 62rem)";

  var accepted = [];
  var ignored = 0;
  var reading = null;

  var idleSnapshot = null;

  function acceptedExtensions() {
    var zone = document.getElementById("drop-zone");
    var raw = zone && zone.dataset ? zone.dataset.acceptExtensions : null;
    return raw ? raw.split(",") : [];
  }

  function isAccepted(file) {
    var exts = acceptedExtensions();
    if (exts.length === 0) return true;
    var name = file.name.toLowerCase();
    return exts.some(function (ext) {
      return name.slice(-ext.length) === ext.toLowerCase();
    });
  }

  function syncAccepted() {
    var select = document.getElementById("operation");
    var zone = document.getElementById("drop-zone");
    if (!select || !zone || !zone.dataset) return;

    var option = select.options[select.selectedIndex];
    var raw = option && option.dataset ? option.dataset.acceptExtensions : null;
    if (!raw) return;

    zone.dataset.acceptExtensions = raw;

    var picker = document.getElementById("file-picker");
    if (picker) picker.setAttribute("accept", raw);

    var phrase = document.querySelector(".accept-list");
    if (phrase) phrase.textContent = raw.split(",").join(" and ");

    var note = document.querySelector(".expansion-note");
    if (note) {
      var text = option.dataset.expansionNote || "";
      note.textContent = text;
      note.hidden = text === "";
    }
  }

  function displayPath(file) {
    return file.mdmasterPath || file.webkitRelativePath || file.name;
  }

  function plural(n, noun) {
    return n + " " + noun + (n === 1 ? "" : "s");
  }

  function humanBytes(n) {
    var units = [["GB", 1073741824], ["MB", 1048576], ["KB", 1024]];
    for (var i = 0; i < units.length; i += 1) {
      var unit = units[i][0];
      var divisor = units[i][1];
      if (n >= divisor) {
        var whole = Math.floor(n / divisor);
        var tenth = Math.floor((n * 10) / divisor) % 10;
        return tenth === 0 ? whole + " " + unit : whole + "." + tenth + " " + unit;
      }
    }
    return n === 1 ? "1 byte" : n + " bytes";
  }

  function zoneLimit(name, fallback) {
    var zone = document.getElementById("drop-zone");
    var raw = zone && zone.dataset ? zone.dataset[name] : null;
    var parsed = raw ? parseInt(raw, 10) : NaN;
    return isNaN(parsed) ? fallback : parsed;
  }

  function maxFileBytes() {
    return zoneLimit("maxFileBytes", Infinity);
  }

  function maxFiles() {
    return zoneLimit("maxFiles", Infinity);
  }

  function addFiles(fileList) {
    var incoming = Array.prototype.slice.call(fileList);
    var usable = incoming.filter(isAccepted);
    ignored += incoming.length - usable.length;

    var seen = Object.create(null);
    accepted.concat(usable).forEach(function (file) {
      seen[displayPath(file)] = file;
    });
    accepted = Object.keys(seen).sort().map(function (key) {
      return seen[key];
    });
    renderAccepted();
  }

  function removeAt(index) {
    accepted.splice(index, 1);
    renderAccepted();
  }

  function clearFiles() {
    accepted = [];
    ignored = 0;
    renderAccepted();
  }

  function renderAccepted() {
    var list = document.getElementById("accepted-files");
    if (list) {
      var limit = maxFileBytes();
      list.textContent = "";
      accepted.forEach(function (file, index) {
        list.appendChild(chipFor(file, index, limit));
      });
    }
    renderSummary();
  }

  function chipFor(file, index, limit) {
    var path = displayPath(file);
    var oversize = file.size > limit;

    var item = document.createElement("li");
    item.className = oversize ? "file-chip is-oversize" : "file-chip";
    if (oversize) {
      item.title = "Larger than the " + humanBytes(limit) + " per-file limit — "
        + "the server will refuse it.";
    }

    var name = document.createElement("span");
    name.className = "chip-path";
    name.textContent = path;
    name.title = path;

    var size = document.createElement("span");
    size.className = "chip-size";
    size.textContent = oversize ? humanBytes(file.size) + " — too big" : humanBytes(file.size);

    var remove = document.createElement("button");
    remove.type = "button";
    remove.className = "chip-remove";
    remove.textContent = "×";
    remove.setAttribute("aria-label", "Remove " + path);
    remove.addEventListener("click", function () {
      removeAt(index);
    });

    item.appendChild(name);
    item.appendChild(size);
    item.appendChild(remove);
    return item;
  }

  function renderSummary() {
    var summary = document.getElementById("file-summary");
    var count = document.getElementById("file-count");
    if (!summary || !count) return;

    if (reading) {
      summary.hidden = false;
      count.textContent = reading === sampleLoad
        ? "Loading the sample files…" : "Reading the dropped files…";
      count.classList.remove("is-warning");
      return;
    }

    if (!accepted.length && !ignored) {
      summary.hidden = true;
      count.textContent = "";
      count.classList.remove("is-warning");
      return;
    }

    var fileLimit = maxFiles();
    var byteLimit = maxFileBytes();
    var total = accepted.reduce(function (sum, file) {
      return sum + file.size;
    }, 0);

    var parts = [];
    if (accepted.length) {
      parts.push(plural(accepted.length, "file") + " ready");
      parts.push(humanBytes(total));
    } else {
      parts.push("nothing selected yet");
    }
    if (ignored) {
      parts.push(plural(ignored, "other file") + " ignored");
    }
    if (accepted.length > fileLimit) {
      parts.push("over the limit of " + plural(fileLimit, "file"));
    }

    var overLimit = accepted.length > fileLimit
      || accepted.some(function (file) { return file.size > byteLimit; });

    summary.hidden = false;
    count.textContent = parts.join(" · ");
    count.classList.toggle("is-warning", overLimit);
  }

  function collectEntries(entry, out) {
    return new Promise(function (resolve) {
      if (entry.isFile) {
        entry.file(function (file) {
          try {
            Object.defineProperty(file, "webkitRelativePath", {
              value: entry.fullPath.replace(/^\//, ""),
            });
          } catch (redefinitionRefused) {
            file.mdmasterPath = entry.fullPath.replace(/^\//, "");
          }
          out.push(file);
          resolve();
        }, resolve);
        return;
      }
      if (!entry.isDirectory) {
        resolve();
        return;
      }
      var reader = entry.createReader();
      var pending = [];
      (function readMore() {
        reader.readEntries(function (entries) {
          if (!entries.length) {
            Promise.all(pending).then(resolve);
            return;
          }
          entries.forEach(function (child) {
            pending.push(collectEntries(child, out));
          });
          readMore();
        }, function () {
          Promise.all(pending).then(resolve);
        });
      })();
    });
  }

  function acceptDrop(event) {
    var items = event.dataTransfer.items;
    var flat = Array.prototype.slice.call(event.dataTransfer.files || []);

    if (items && items.length && items[0].webkitGetAsEntry) {
      var collected = [];
      var walks = [];
      for (var i = 0; i < items.length; i += 1) {
        var entry = items[i].webkitGetAsEntry();
        if (entry) walks.push(collectEntries(entry, collected));
      }
      reading = Promise.all(walks).then(function () {
        reading = null;
        addFiles(walks.length ? collected : flat);
      });
      renderSummary();
    } else {
      addFiles(flat);
    }
  }

  function wireDropZone() {
    var zone = document.getElementById("drop-zone");
    if (!zone) return;

    var depth = 0;

    zone.addEventListener("dragenter", function (event) {
      event.preventDefault();
      depth += 1;
      zone.classList.add("dragging");
    });

    zone.addEventListener("dragover", function (event) {
      event.preventDefault();
      if (event.dataTransfer) event.dataTransfer.dropEffect = "copy";
    });

    zone.addEventListener("dragleave", function () {
      depth = Math.max(0, depth - 1);
      if (depth === 0) zone.classList.remove("dragging");
    });

    zone.addEventListener("drop", function (event) {
      event.preventDefault();
      depth = 0;
      zone.classList.remove("dragging");
      acceptDrop(event);
    });

    ["file-picker", "folder-picker"].forEach(function (id) {
      var input = document.getElementById(id);
      if (!input) return;
      input.addEventListener("change", function () {
        addFiles(input.files);
        input.value = "";
      });
    });
  }

  function dragCarriesFiles(event) {
    var transfer = event.dataTransfer;
    if (!transfer || !transfer.types) return false;
    return Array.prototype.indexOf.call(transfer.types, "Files") !== -1;
  }

  function wireDocumentDrop() {
    var overlay = document.getElementById("drop-overlay");
    var depth = 0;

    var quietTimer = null;

    function showOverlay() {
      if (!overlay) return;
      document.body.classList.add("dragging");
      if (quietTimer) clearTimeout(quietTimer);
      quietTimer = setTimeout(hideOverlay, 1500);
    }

    function hideOverlay() {
      depth = 0;
      if (quietTimer) {
        clearTimeout(quietTimer);
        quietTimer = null;
      }
      document.body.classList.remove("dragging");
    }

    document.addEventListener("dragenter", function (event) {
      if (!dragCarriesFiles(event)) return;
      event.preventDefault();
      depth += 1;
      showOverlay();
    });

    document.addEventListener("dragover", function (event) {
      if (!dragCarriesFiles(event)) return;
      event.preventDefault();
      if (event.dataTransfer) event.dataTransfer.dropEffect = "copy";
      showOverlay();
    });

    document.addEventListener("dragleave", function (event) {
      if (!dragCarriesFiles(event)) return;
      depth = Math.max(0, depth - 1);
      if (depth === 0) hideOverlay();
    });

    document.addEventListener("drop", function (event) {
      if (!dragCarriesFiles(event)) return;
      event.preventDefault();
      hideOverlay();

      if (event.target && event.target.closest
        && event.target.closest("#drop-zone")) return;
      acceptDrop(event);
    });
  }

  var sampleFiles = [];
  var sampleLoad = null;

  function sampleSet(operation) {
    var sets = document.querySelectorAll("[data-sample-operation]");
    return Array.prototype.filter.call(sets, function (set) {
      return set.dataset.sampleOperation === operation;
    })[0] || null;
  }

  function fetchSample(link) {
    return fetch(link.href).then(function (response) {
      if (!response.ok) throw new Error("sample refused");
      return response.blob();
    }).then(function (blob) {
      var path = link.textContent;
      var file = new File([blob], path.slice(path.lastIndexOf("/") + 1), { type: blob.type });
      file.mdmasterPath = path;
      return file;
    });
  }

  function onlySamples() {
    if (sampleLoad) return true;
    return accepted.length > 0 && accepted.every(function (file) {
      return sampleFiles.indexOf(file) !== -1;
    });
  }

  function loadSamples(operation) {
    var set = sampleSet(operation);
    if (!set) return;
    var links = Array.prototype.slice.call(set.querySelectorAll("a.sample-file"));
    var before = accepted;
    var load = Promise.all(links.map(fetchSample)).then(function (files) {
      if (sampleLoad !== load) return;
      settleSamples(load);
      if (accepted === before) {
        sampleFiles = files;
        accepted = [];
        ignored = 0;
        addFiles(files);
      } else {
        renderSummary();
      }
    }, function () {
      if (sampleLoad !== load) return;
      settleSamples(load);
      renderSummary();
      resultsBanner("The sample files could not be loaded. Try again in a moment.");
    });
    sampleLoad = load;
    reading = load;
    renderSummary();
  }

  function settleSamples(load) {
    sampleLoad = null;
    if (reading === load) reading = null;
  }

  function chooseSamples(operation) {
    var select = document.getElementById("operation");
    if (!select) return;
    sampleLoad = null;
    reading = null;
    clearFiles();
    if (select.value !== operation) {
      select.value = operation;
      select.dispatchEvent(new Event("change", { bubbles: true }));
    }
    loadSamples(operation);
  }

  var elapsedTimer = null;
  var startedAt = null;

  function stopElapsed() {
    if (elapsedTimer) {
      clearInterval(elapsedTimer);
      elapsedTimer = null;
    }
  }

  function updateElapsed(label) {
    var seconds = Math.floor((Date.now() - startedAt) / 1000);
    label.textContent = "Working… " + seconds + " s";
  }

  function startElapsed(label) {
    stopElapsed();
    startedAt = Date.now();
    updateElapsed(label);
    elapsedTimer = setInterval(function () {
      updateElapsed(label);
    }, 1000);
  }

  function setRunning(form, running) {
    form.classList.toggle("running", running);
    var button = form.querySelector(".run-button");
    if (button) {
      button.disabled = running;
      button.setAttribute("aria-busy", running ? "true" : "false");
    }
    if (!running) stopElapsed();
    var label = form.querySelector(".run-label");
    if (!label) return;
    if (running) {
      label.dataset.idle = label.textContent;
      startElapsed(label);
    } else if (label.dataset.idle) {
      label.textContent = label.dataset.idle;
    }
  }

  function clearFormErrors() {
    var panel = document.getElementById("form-panel");
    if (!panel) return;
    Array.prototype.forEach.call(
      panel.querySelectorAll(".field-error, .error-banner"),
      function (node) {
        node.remove();
      }
    );
  }

  function idleResults() {
    var results = document.getElementById("results");
    if (!results || results.querySelector(".file-list")) return;
    if (!idleSnapshot) return;
    results.replaceWith(idleSnapshot.cloneNode(true));
    watchResultsExpiry();
  }

  function resultsBanner(message) {
    var results = document.getElementById("results");
    var template = document.getElementById("error-banner-template");
    if (!results || !template) return;
    results.className = "results";
    results.textContent = "";

    var banner = template.content.cloneNode(true);
    var text = banner.querySelector(".banner-message");
    if (text) text.textContent = message;
    results.appendChild(banner);
  }

  function swapIn(replacement) {
    var current = document.getElementById(replacement.id);
    if (!current) return false;
    current.replaceWith(replacement);
    if (window.htmx) window.htmx.process(replacement);
    return true;
  }

  function applyResponse(html) {
    var parsed = document.createElement("template");
    parsed.innerHTML = html;

    var panel = parsed.content.querySelector("#form-panel");
    if (panel && swapIn(panel)) {
      syncAccepted();
      var banner = panel.querySelector(".error-banner") || panel;
      banner.scrollIntoView({ behavior: "smooth", block: "nearest" });
      return true;
    }

    var results = parsed.content.querySelector("#results");
    if (results && swapIn(results)) {
      results.scrollIntoView({ behavior: "smooth", block: "nearest" });
      watchResultsExpiry();
      return true;
    }

    return false;
  }

  function retryAfterSeconds(response) {
    var raw = response.headers.get("Retry-After");
    if (!raw) return null;
    if (/^\s*\d+\s*$/.test(raw)) return parseInt(raw, 10);
    var at = Date.parse(raw);
    if (isNaN(at)) return null;
    return Math.max(0, Math.ceil((at - Date.now()) / 1000));
  }

  function statusMessage(response) {
    var status = response.status;
    if (status === 429) {
      var wait = retryAfterSeconds(response);
      return "The server is busy with other runs right now. Try again "
        + (wait ? "in " + plural(wait, "second") : "in a moment") + ".";
    }
    if (status === 413) {
      return "The upload is too large for the server, or for the proxy in "
        + "front of it. Try fewer or smaller files.";
    }
    if (status === 502 || status === 503 || status === 504) {
      return "The server could not be reached, or is restarting. "
        + "Try again in a moment.";
    }
    return "The server sent a response this page did not understand"
      + (response.ok ? "." : " (HTTP " + status + ").");
  }

  function submitRun(form) {
    if (reading) {
      reading.then(function () {
        submitRun(form);
      });
      return;
    }

    clearFormErrors();
    idleResults();

    if (!accepted.length) {
      resultsBanner("No files selected yet — drop some above, or use the buttons.");
      return;
    }

    var body = new FormData(form);
    accepted.forEach(function (file) {
      var path = displayPath(file);
      body.append("files", file, path);
      body.append("paths", path);
    });

    setRunning(form, true);
    fetch(form.getAttribute("action"), { method: "POST", body: body })
      .then(function (response) {
        return response.text().then(function (text) {
          if (!applyResponse(text)) resultsBanner(statusMessage(response));
        });
      })
      .catch(function () {
        resultsBanner("The upload failed. Check your connection and try again.");
      })
      .finally(function () {
        setRunning(form, false);
      });
  }

  var expiryTimer = null;

  function expirySentence(minutes) {
    return "Available for download for up to " + plural(minutes, "more minute") +
      " (sooner if the server needs the room, so save what you need now).";
  }

  function stopResultsExpiry() {
    if (expiryTimer) {
      clearInterval(expiryTimer);
      expiryTimer = null;
    }
  }

  function watchResultsExpiry() {
    stopResultsExpiry();
    var results = document.getElementById("results");
    if (!results || !results.dataset || !results.dataset.expiresAt) return;
    updateResultsExpiry();
    expiryTimer = setInterval(updateResultsExpiry, 30000);
  }

  function updateResultsExpiry() {
    var results = document.getElementById("results");
    var raw = results && results.dataset ? results.dataset.expiresAt : null;
    if (!raw) return;

    var line = document.querySelector("#results .results-expiry");

    var minutes = Math.ceil((new Date(raw).getTime() - Date.now()) / 60000);
    if (minutes > 0) {
      if (line) line.textContent = expirySentence(minutes);
      return;
    }

    stopResultsExpiry();
    if (line) {
      line.textContent = "This result has expired — upload your files again.";
      line.classList.add("is-expired");
    }
    Array.prototype.forEach.call(
      results.querySelectorAll("a.download-all, a.file-download"),
      function (link) {
        link.removeAttribute("href");
        link.setAttribute("aria-disabled", "true");
        link.classList.add("is-expired");
      }
    );
  }

  function applyFileFilter(input) {
    var results = document.getElementById("results");
    if (!results) return;

    var query = input.value.trim().toLowerCase();
    var rows = results.querySelectorAll(".file-list .file");
    var shown = 0;
    Array.prototype.forEach.call(rows, function (row) {
      var path = row.dataset && row.dataset.path ? row.dataset.path : "";
      var hit = query === "" || path.toLowerCase().indexOf(query) !== -1;
      row.classList.toggle("is-hidden", !hit);
      if (hit) shown += 1;
    });

    var count = results.querySelector(".file-filter-count");
    if (count) {
      count.hidden = query === "";
      count.textContent = query === "" ? "" : shown + " of " + rows.length + " shown";
    }
    var nomatch = results.querySelector(".file-filter-nomatch");
    if (nomatch) nomatch.hidden = !(query !== "" && shown === 0);
  }

  function openableRows() {
    var results = document.getElementById("results");
    if (!results) return [];
    return Array.prototype.filter.call(
      results.querySelectorAll(".file-list .file"),
      function (row) {
        return !row.classList.contains("is-hidden")
          && row.querySelector("button.file-name");
      }
    );
  }

  function selectedAt(rows) {
    for (var i = 0; i < rows.length; i += 1) {
      if (rows[i].classList.contains("is-active")) return i;
    }
    return -1;
  }

  function stepTo(rows, index) {
    if (index < 0 || index >= rows.length) return;
    var button = rows[index].querySelector("button.file-name");
    if (button) button.click();
  }

  var copyTimer = null;

  var copyLabel = "Copy";

  function resetCopy(button) {
    if (copyTimer) {
      clearTimeout(copyTimer);
      copyTimer = null;
    }
    button.textContent = copyLabel;
    button.classList.remove("is-copied");
  }

  function copyBanner(message) {
    var pane = document.getElementById("diff-pane");
    var template = document.getElementById("error-banner-template");
    if (!pane || !template) return;
    dropCopyBanner();

    var banner = template.content.cloneNode(true);
    var text = banner.querySelector(".banner-message");
    if (text) text.textContent = message;
    pane.insertBefore(banner, pane.firstChild);
  }

  function dropCopyBanner() {
    var pane = document.getElementById("diff-pane");
    var previous = pane && pane.querySelector(".error-banner");
    if (previous) previous.remove();
  }

  function copyTransformed(button) {
    resetCopy(button);
    dropCopyBanner();

    var url = button.dataset.copyHref;
    if (!url) {
      copyBanner("The copy button named no download to fetch.");
      return;
    }

    if (!navigator.clipboard || !navigator.clipboard.writeText) {
      copyBanner("This browser has no clipboard to copy to.");
      return;
    }

    fetch(url).then(function (response) {
      if (!response.ok) {
        copyBanner("The server refused the download — the result may have expired.");
        return;
      }
      response.text().then(function (text) {
        navigator.clipboard.writeText(text).then(function () {
          button.textContent = "Copied";
          button.classList.add("is-copied");
          copyTimer = setTimeout(function () {
            copyTimer = null;
            button.textContent = copyLabel;
            button.classList.remove("is-copied");
          }, 2000);
        }, function () {
          copyBanner("The browser refused the copy.");
        });
      }, function () {
        copyBanner("The download could not be read.");
      });
    }, function () {
      copyBanner("The download failed. Check your connection and try again.");
    });
  }

  var SETTINGS_KEY = "mdmaster.settings";

  var restoring = false;
  var pendingSettings = null;

  function optionControls() {
    var panel = document.getElementById("operation-options");
    return panel ? panel.querySelectorAll("input, select") : [];
  }

  function currentSettings() {
    var select = document.getElementById("operation");
    var options = {};
    Array.prototype.forEach.call(optionControls(), function (control) {
      if (control.name) options[control.name] = control.value;
    });
    return { operation: select ? select.value : "", options: options };
  }

  function saveSettings() {
    if (restoring) return;
    try {
      localStorage.setItem(SETTINGS_KEY, JSON.stringify(currentSettings()));
    } catch (storageRefused) {
      // The browser refused to save the operation settings (private mode,
      // full storage); they are not remembered, and the run goes out anyway.
    }
  }

  function readSettings() {
    var raw;
    try {
      raw = localStorage.getItem(SETTINGS_KEY);
    } catch (storageRefused) {
      return null;
    }
    if (!raw) return null;

    var saved;
    try {
      saved = JSON.parse(raw);
    } catch (unreadable) {
      return null;
    }
    if (!saved || typeof saved !== "object") return null;
    if (typeof saved.operation !== "string" || !saved.operation) return null;
    if (!saved.options || typeof saved.options !== "object") return null;
    return saved;
  }

  function choicesInclude(collection, value) {
    for (var i = 0; i < collection.length; i += 1) {
      if (collection[i].value === value) return true;
    }
    return false;
  }

  function applyFieldValues(options) {
    Array.prototype.forEach.call(optionControls(), function (control) {
      if (!control.name || !Object.prototype.hasOwnProperty.call(options, control.name)) return;
      var value = options[control.name];
      if (typeof value !== "string") return;
      if (control.tagName === "SELECT" && !choicesInclude(control.options, value)) return;
      control.value = value;
    });
  }

  function restoreSettings() {
    var saved = readSettings();
    var select = document.getElementById("operation");
    if (!saved || !select || !choicesInclude(select.options, saved.operation)) return;

    if (select.value === saved.operation) {
      applyFieldValues(saved.options);
      syncAccepted();
      return;
    }

    select.value = saved.operation;
    restoring = true;
    select.dispatchEvent(new Event("change", { bubbles: true }));
    restoring = false;
    pendingSettings = saved.options;
  }

  function wireSettings() {
    document.addEventListener("htmx:afterSwap", function (event) {
      if (!event.target || event.target.id !== "operation-options") return;
      if (pendingSettings) {
        var held = pendingSettings;
        pendingSettings = null;
        applyFieldValues(held);
      }
      saveSettings();
    });
  }

  function wireDelegates() {
    document.addEventListener("submit", function (event) {
      if (!event.target || event.target.id !== "run-form") return;
      event.preventDefault();
      saveSettings();
      submitRun(event.target);
    });

    document.addEventListener("change", function (event) {
      if (!event.target || event.target.id !== "operation") return;
      syncAccepted();
      pendingSettings = null;
      saveSettings();
      if (!restoring && onlySamples()) loadSamples(event.target.value);
    });

    document.addEventListener("input", function (event) {
      if (!event.target || event.target.id !== "file-filter") return;
      applyFileFilter(event.target);
    });

    document.addEventListener("click", function (event) {
      var target = event.target;
      if (!target || !target.closest) return;

      if (target.closest("#clear-files")) {
        clearFiles();
        return;
      }

      var sample = target.closest("button.sample-load");
      if (sample) {
        chooseSamples(sample.closest("[data-sample-operation]").dataset.sampleOperation);
        return;
      }

      var copy = target.closest("button.diff-copy");
      if (copy) {
        copyTransformed(copy);
        return;
      }

      var name = target.closest("button.file-name");
      if (!name) return;
      var list = name.closest(".file-list");
      if (list) {
        Array.prototype.forEach.call(list.querySelectorAll(".is-active"), function (row) {
          row.classList.remove("is-active");
        });
      }
      var row = name.closest(".file");
      if (row) row.classList.add("is-active");
    });

    document.addEventListener("keydown", function (event) {
      var focused = document.activeElement;
      if (focused && (focused.tagName === "INPUT" || focused.tagName === "TEXTAREA"
        || focused.tagName === "SELECT")) return;
      if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) return;

      var key = event.key.toLowerCase();
      var rows = openableRows();
      if (!rows.length) return;

      var at = selectedAt(rows);
      var to;
      if (key === "arrowdown" || key === "j") to = at < 0 ? 0 : at + 1;
      else if (key === "arrowup" || key === "k") to = at < 0 ? rows.length - 1 : at - 1;
      else if (key === "home") to = 0;
      else if (key === "end") to = rows.length - 1;
      else return;

      event.preventDefault();
      stepTo(rows, to);
    });

    document.addEventListener("htmx:afterSwap", function (event) {
      if (!event.target || event.target.id !== "diff-pane") return;
      var heading = event.target.querySelector(".diff-title");
      if (heading) heading.focus({ preventScroll: true });
      if (window.matchMedia && window.matchMedia(WIDE).matches) return;
      event.target.scrollIntoView({ behavior: "smooth", block: "start" });
    });
  }

  document.addEventListener("DOMContentLoaded", function () {
    wireDropZone();
    wireDocumentDrop();
    wireDelegates();
    wireSettings();
    syncAccepted();
    restoreSettings();
    var picker = document.getElementById("operation");
    if (picker) loadSamples(picker.value);
    idleSnapshot = (function () {
      var results = document.getElementById("results");
      return results && !results.querySelector(".file-list")
        ? results.cloneNode(true)
        : null;
    })();
    watchResultsExpiry();
  });
})();
