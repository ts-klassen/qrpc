(function (global) {
    'use strict';

    const DEFAULT_ENDPOINT = '/qrpc';
    const SETTINGS_KEY = 'qrpc_vue_settings';
    const DEFAULT_SETTINGS = {
        language: 'en',
        errorDisplay: 'toast'
    };
    const HISTORY_LIMIT = 20;
    const CLIENT_ERROR_NAMESPACE = 'qrpc_client';

    function createRpcCaller(endpoint, { onError }) {
        return async function callRpc({ module, function: func, arity = 1, payload = {} }) {
            if (!module || !func) {
                throw new Error('Both module and function are required.');
            }

            const body = JSON.stringify({
                metadata: {
                    module,
                    function: func,
                    arity
                },
                payload
            });

            try {
                let response;
                try {
                    response = await fetch(endpoint, {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json'
                        },
                        body
                    });
                } catch (networkError) {
                    throw createNetworkTransportError(networkError);
                }

                if (response.status !== 200) {
                    throw createHttpStatusError(response);
                }

                let rawBody;
                try {
                    rawBody = await response.text();
                } catch (readError) {
                    throw createNetworkTransportError(readError);
                }

                const parseResult = parseResponseBody(rawBody);
                let result = null;
                if (parseResult.ok) {
                    result = parseResult.value;
                } else {
                    throw createResponseParseError(parseResult.error);
                }

                const success = result?.metadata?.success;
                if (success !== undefined && success !== true) {
                    throw createQrpcResponseError(result);
                }

                if (!result && result !== null) {
                    result = null;
                }

                return result;
            } catch (error) {
                const normalized = ensureQrpcError(error);
                if (typeof onError === 'function') {
                    onError(normalized);
                }
                throw normalized;
            }
        };
    }

    const QrpcVuePlugin = {
        install(app, options = {}) {
            const endpoint = options.endpoint || DEFAULT_ENDPOINT;
            const settingsManager = createSettingsManager();
            const historyManager = createHistoryManager();
            const toast = createToastContainer();
            const modal = createModalContainer();
            const ui = createSettingsUi(settingsManager, historyManager, modal);

            const onError = (error) => {
                const settings = settingsManager.get();
                const message = resolveErrorMessage(error, settings.language);
                historyManager.add({
                    timestamp: Date.now(),
                    messages: {
                        en: resolveErrorMessage(error, 'en'),
                        ja: resolveErrorMessage(error, 'ja')
                    },
                    metadata: error.metadata || null,
                    detail: error?.metadata?.error_detail || null
                });
                switch (settings.errorDisplay) {
                    case 'toast':
                        hideModal(modal);
                        showToast(toast, message);
                        break;
                    case 'modal':
                        const { content, hasStructuredDetail } = buildErrorDetailContent({
                            detail: error?.metadata?.error_detail,
                            language: settings.language,
                            fallbackText: message
                        });
                        showModal(
                            modal,
                            content,
                            settings.language,
                            hasStructuredDetail ? { titleText: '' } : undefined
                        );
                        break;
                    case 'none':
                    default:
                        hideModal(modal);
                        break;
                }
            };

            const callRpc = createRpcCaller(endpoint, { onError });
            app.config.globalProperties.$qrpc = {
                callRpc,
                settings: {
                    get: () => settingsManager.get(),
                    set: (key, value) => settingsManager.set(key, value),
                    subscribe: (fn) => settingsManager.subscribe(fn)
                },
                history: {
                    get: () => historyManager.get(),
                    subscribe: (fn) => historyManager.subscribe(fn),
                    clear: () => historyManager.clear()
                }
            };

            app.config.globalProperties.$qrpcUi = ui;

            settingsManager.subscribe((settings) => {
                if (settings.errorDisplay !== 'modal') {
                    hideModal(modal);
                }
            });
        }
    };

    function createSettingsManager() {
        let current = loadSettings();
        const subscribers = [];

        const snapshot = () => Object.assign({}, current);

        return {
            get: () => snapshot(),
            set: (key, value) => {
                if (!Object.prototype.hasOwnProperty.call(DEFAULT_SETTINGS, key)) {
                    return snapshot();
                }
                current = Object.assign({}, current, { [key]: value });
                saveSettings(current);
                const next = snapshot();
                subscribers.forEach((fn) => {
                    try {
                        fn(next);
                    } catch (_) {
                        // ignore subscriber errors
                    }
                });
                return next;
            },
            subscribe: (fn) => {
                if (typeof fn === 'function') {
                    subscribers.push(fn);
                    fn(snapshot());
                }
                return () => {
                    const idx = subscribers.indexOf(fn);
                    if (idx >= 0) {
                        subscribers.splice(idx, 1);
                    }
                };
            }
        };
    }

    function createHistoryManager(limit = HISTORY_LIMIT) {
        let entries = [];
        const subscribers = [];

        const snapshot = () => entries.slice();
        const notify = () => {
            const current = snapshot();
            subscribers.forEach((fn) => {
                try {
                    fn(current);
                } catch (_) {
                    // ignore subscriber errors
                }
            });
        };

        return {
            get: () => snapshot(),
            add: (entry) => {
                if (!entry) {
                    return snapshot();
                }
                const cloned = {
                    timestamp: entry.timestamp,
                    messages: Object.assign({}, entry.messages || {}),
                    metadata: entry.metadata || null,
                    detail: entry.detail || null
                };
                entries = entries.concat(cloned);
                if (entries.length > limit) {
                    entries = entries.slice(entries.length - limit);
                }
                notify();
                return snapshot();
            },
            clear: () => {
                entries = [];
                notify();
                return [];
            },
            subscribe: (fn) => {
                if (typeof fn === 'function') {
                    subscribers.push(fn);
                    fn(snapshot());
                }
                return () => {
                    const idx = subscribers.indexOf(fn);
                    if (idx >= 0) {
                        subscribers.splice(idx, 1);
                    }
                };
            }
        };
    }

    function loadSettings() {
        try {
            const raw = global.localStorage && global.localStorage.getItem(SETTINGS_KEY);
            if (!raw) {
                return Object.assign({}, DEFAULT_SETTINGS);
            }
            const parsed = JSON.parse(raw);
            const sanitized = Object.assign({}, DEFAULT_SETTINGS);
            Object.keys(DEFAULT_SETTINGS).forEach((key) => {
                if (Object.prototype.hasOwnProperty.call(parsed, key)) {
                    sanitized[key] = parsed[key];
                }
            });
            return sanitized;
        } catch (_) {
            return Object.assign({}, DEFAULT_SETTINGS);
        }
    }

    function saveSettings(settings) {
        try {
            if (global.localStorage) {
                const payload = Object.assign({}, DEFAULT_SETTINGS);
                Object.keys(DEFAULT_SETTINGS).forEach((key) => {
                    if (Object.prototype.hasOwnProperty.call(settings, key)) {
                        payload[key] = settings[key];
                    }
                });
                global.localStorage.setItem(SETTINGS_KEY, JSON.stringify(payload));
            }
        } catch (_) {
            // ignore persistence failures
        }
    }

    function createSettingsUi(settingsManager, historyManager, errorModalOverlay) {
        const rootId = 'qrpc-settings-root';
        let root = document.getElementById(rootId);
        if (root) {
            return root;
        }

        root = document.createElement('div');
        root.id = rootId;
        root.style.position = 'fixed';
        root.style.bottom = '16px';
        root.style.right = '16px';
        root.style.zIndex = '10000';
        root.style.display = 'flex';
        root.style.flexDirection = 'column';
        root.style.alignItems = 'flex-end';
        root.style.fontFamily = 'sans-serif';
        root.style.fontSize = '14px';

        const menuButton = document.createElement('button');
        menuButton.textContent = 'QRPC Menu';
        styleButton(menuButton);

        const panel = document.createElement('div');
        panel.style.marginTop = '8px';
        panel.style.backgroundColor = '#ffffff';
        panel.style.border = '1px solid #ccc';
        panel.style.borderRadius = '4px';
        panel.style.boxShadow = '0 2px 8px rgba(0, 0, 0, 0.15)';
        panel.style.padding = '12px';
        panel.style.minWidth = '240px';
        panel.style.display = 'none';
        panel.style.color = '#333';

        const modalBackdrop = document.createElement('div');
        modalBackdrop.style.position = 'fixed';
        modalBackdrop.style.top = '0';
        modalBackdrop.style.left = '0';
        modalBackdrop.style.width = '100%';
        modalBackdrop.style.height = '100%';
        modalBackdrop.style.display = 'none';
        modalBackdrop.style.alignItems = 'center';
        modalBackdrop.style.justifyContent = 'center';
        modalBackdrop.style.backgroundColor = 'rgba(0, 0, 0, 0.45)';
        modalBackdrop.style.zIndex = '10002';
        modalBackdrop.style.padding = '16px';
        modalBackdrop.style.boxSizing = 'border-box';

        let detailModal = errorModalOverlay;
        if (!detailModal) {
            detailModal = createModalContainer();
        }

        const tabs = document.createElement('div');
        tabs.style.display = 'flex';
        tabs.style.gap = '12px';
        tabs.style.marginBottom = '12px';
        tabs.style.borderBottom = '1px solid #e0e0e0';
        tabs.style.paddingBottom = '8px';

        const settingsTab = document.createElement('span');
        settingsTab.textContent = 'Preferences';
        settingsTab.style.cursor = 'pointer';

        const historyTab = document.createElement('span');
        historyTab.textContent = 'Error History';
        historyTab.style.cursor = 'pointer';

        const setTabActiveStyles = (tab, isActive) => {
            tab.style.fontWeight = isActive ? 'bold' : 'normal';
            tab.style.color = isActive ? '#007bff' : '#333';
        };

        const settingsContent = document.createElement('div');

        const heading = document.createElement('div');
        heading.textContent = 'Preferences';
        heading.style.fontWeight = 'bold';
        heading.style.marginBottom = '8px';

        const languageRow = createSelectRow({
            label: 'Language',
            value: settingsManager.get().language,
            options: [
                { value: 'en', label: 'en' },
                { value: 'ja', label: 'ja' }
            ],
            onChange: (value) => settingsManager.set('language', value)
        });

        const errorDisplayRow = createSelectRow({
            label: 'Display',
            value: settingsManager.get().errorDisplay,
            options: [
                { value: 'toast', label: 'toast' },
                { value: 'modal', label: 'modal' },
                { value: 'none', label: 'none' }
            ],
            onChange: (value) => settingsManager.set('errorDisplay', value)
        });

        const closeButton = document.createElement('button');
        closeButton.textContent = 'Close';
        styleButton(closeButton);
        closeButton.style.marginTop = '12px';

        const historyContent = document.createElement('div');
        historyContent.style.display = 'none';
        historyContent.style.maxWidth = '320px';
        historyContent.style.maxHeight = '320px';
        historyContent.style.overflowY = 'auto';

        const historyHeading = document.createElement('div');
        historyHeading.textContent = 'Error History';
        historyHeading.style.fontWeight = 'bold';
        historyHeading.style.marginBottom = '8px';

        const historyList = document.createElement('div');
        historyList.style.display = 'flex';
        historyList.style.flexDirection = 'column';
        historyList.style.gap = '8px';

        const historyControls = document.createElement('div');
        historyControls.style.display = 'flex';
        historyControls.style.justifyContent = 'flex-end';
        historyControls.style.gap = '8px';
        historyControls.style.marginTop = '12px';

        const historyClearButton = document.createElement('button');
        historyClearButton.textContent = 'Clear History';
        styleButton(historyClearButton);
        historyClearButton.style.marginTop = '0';

        historyControls.appendChild(historyClearButton);

        panel.appendChild(tabs);
        settingsContent.appendChild(heading);
        settingsContent.appendChild(languageRow.container);
        settingsContent.appendChild(errorDisplayRow.container);
        historyContent.appendChild(historyHeading);
        historyContent.appendChild(historyList);
        historyContent.appendChild(historyControls);
        panel.appendChild(settingsContent);
        panel.appendChild(historyContent);
        panel.appendChild(closeButton);

        tabs.appendChild(settingsTab);
        tabs.appendChild(historyTab);

        root.appendChild(menuButton);

        let currentHistory = historyManager.get();

        let activeSection = 'settings';
        let currentDisplayMode = settingsManager.get().errorDisplay === 'modal' ? 'modal' : 'inline';

        const isPanelVisible = () => {
            return currentDisplayMode === 'modal'
                ? modalBackdrop.style.display !== 'none'
                : panel.style.display !== 'none';
        };

        const closePanel = () => {
            panel.style.display = 'none';
            modalBackdrop.style.display = 'none';
        };

        const openPanel = () => {
            if (currentDisplayMode === 'modal') {
                panel.style.display = 'block';
                modalBackdrop.style.display = 'flex';
            } else {
                panel.style.display = 'block';
            }
            if (activeSection === 'history') {
                renderHistory();
            }
        };

        const applyDisplayMode = (mode, options = {}) => {
            const { preserveVisibility = false } = options;
            const wasVisible = preserveVisibility && isPanelVisible();
            const nextMode = mode === 'modal' ? 'modal' : 'inline';
            currentDisplayMode = nextMode;
            if (nextMode === 'modal') {
                if (panel.parentNode !== modalBackdrop) {
                    if (panel.parentNode) {
                        panel.parentNode.removeChild(panel);
                    }
                    modalBackdrop.appendChild(panel);
                }
                panel.style.marginTop = '0';
                panel.style.maxWidth = '360px';
            } else {
                if (panel.parentNode !== root) {
                    if (panel.parentNode) {
                        panel.parentNode.removeChild(panel);
                    }
                    root.appendChild(panel);
                }
                panel.style.marginTop = '8px';
                panel.style.maxWidth = '';
                modalBackdrop.style.display = 'none';
            }
            if (preserveVisibility && wasVisible) {
                openPanel();
            } else {
                closePanel();
            }
        };

        const showSection = (section) => {
            activeSection = section;
            const viewingHistory = section === 'history';
            settingsContent.style.display = viewingHistory ? 'none' : 'block';
            historyContent.style.display = viewingHistory ? 'block' : 'none';
            setTabActiveStyles(settingsTab, !viewingHistory);
            setTabActiveStyles(historyTab, viewingHistory);
            if (viewingHistory && isPanelVisible()) {
                renderHistory();
            }
        };

        settingsTab.addEventListener('click', () => {
            showSection('settings');
        });
        historyTab.addEventListener('click', () => {
            showSection('history');
        });

        menuButton.addEventListener('click', () => {
            if (isPanelVisible()) {
                closePanel();
            } else {
                openPanel();
            }
        });

        closeButton.addEventListener('click', () => {
            closePanel();
        });

        modalBackdrop.addEventListener('click', (event) => {
            if (event.target === modalBackdrop) {
                closePanel();
            }
        });

        const applyLanguage = (language) => {
            const lang = language === 'ja' ? 'ja' : 'en';
            menuButton.textContent = lang === 'ja' ? 'QRPC メニュー' : 'QRPC Menu';
            settingsTab.textContent = lang === 'ja' ? '設定' : 'Preferences';
            historyTab.textContent = lang === 'ja' ? 'エラーログ' : 'Error History';
            heading.textContent = lang === 'ja' ? '設定' : 'Preferences';
            languageRow.label.textContent = lang === 'ja' ? '言語' : 'Language';
            errorDisplayRow.label.textContent = lang === 'ja' ? '表示' : 'Display';
            closeButton.textContent = lang === 'ja' ? '閉じる' : 'Close';
            historyHeading.textContent = lang === 'ja' ? 'エラーログ' : 'Error History';
            historyClearButton.textContent = lang === 'ja' ? '履歴をクリア' : 'Clear History';
        };

        const stringifyDetail = (value) => {
            if (value === null || typeof value === 'undefined') {
                return '';
            }
            if (typeof value === 'string') {
                return value;
            }
            try {
                return JSON.stringify(value, null, 2);
            } catch (_) {
                return String(value);
            }
        };

        const formatHistoryDetail = (entry, language) => {
            const lang = language === 'ja' ? 'ja' : 'en';
            const metadataText = stringifyDetail(entry.metadata);
            if (metadataText) {
                return metadataText;
            }
            const detailText = stringifyDetail(entry.detail);
            if (detailText) {
                return detailText;
            }
            const fallback = entry.messages?.[lang] || entry.messages?.en || '';
            if (fallback) {
                return fallback;
            }
            return lang === 'ja' ? '詳細はありません。' : 'No additional detail available.';
        };

        const renderHistory = () => {
            const language = settingsManager.get().language;
            const locale = language === 'ja' ? 'ja-JP' : 'en-US';
            historyList.innerHTML = '';
            if (!currentHistory.length) {
                const empty = document.createElement('div');
                empty.textContent = language === 'ja' ? 'エラーはまだありません。' : 'No errors yet.';
                historyList.appendChild(empty);
                return;
            }
            const reversed = currentHistory.slice().reverse();
            reversed.forEach((entry) => {
                const item = document.createElement('div');
                item.style.border = '1px solid #eee';
                item.style.borderRadius = '4px';
                item.style.padding = '8px';
                item.style.backgroundColor = '#f8f9fa';
                item.style.display = 'flex';
                item.style.flexDirection = 'column';
                item.style.gap = '4px';
                item.style.cursor = 'pointer';
                item.setAttribute('role', 'button');
                item.tabIndex = 0;

                const ts = document.createElement('div');
                ts.style.fontSize = '12px';
                ts.style.color = '#555';
                ts.textContent = formatTimestamp(entry.timestamp, locale);

                const message = document.createElement('div');
                message.textContent = entry.messages?.[language] ||
                    entry.messages?.en ||
                    '';

                item.appendChild(ts);
                item.appendChild(message);
                historyList.appendChild(item);

                const openDetail = () => {
                    if (!detailModal) {
                        return;
                    }
                    const activeLanguage = settingsManager.get().language;
                    const fallbackText = formatHistoryDetail(entry, activeLanguage);
                    const { content, hasStructuredDetail } = buildErrorDetailContent({
                        detail: entry?.metadata?.error_detail,
                        language: activeLanguage,
                        fallbackText
                    });
                    showModal(
                        detailModal,
                        content,
                        activeLanguage,
                        hasStructuredDetail ? { titleText: '' } : undefined
                    );
                };

                item.addEventListener('click', () => {
                    openDetail();
                });
                item.addEventListener('keydown', (event) => {
                    if (event.key === 'Enter' || event.key === ' ') {
                        event.preventDefault();
                        openDetail();
                    }
                });
            });
        };

        historyClearButton.addEventListener('click', () => {
            historyManager.clear();
        });

        document.body.appendChild(root);
        document.body.appendChild(modalBackdrop);
        applyDisplayMode(settingsManager.get().errorDisplay);

        document.addEventListener('keydown', (event) => {
            if (event.key === 'Escape') {
                closePanel();
            }
        });

        historyManager.subscribe((entries) => {
            currentHistory = entries;
            if (isPanelVisible() && activeSection === 'history') {
                renderHistory();
            }
        });

        settingsManager.subscribe((settings) => {
            languageRow.select.value = settings.language;
            errorDisplayRow.select.value = settings.errorDisplay;
            applyDisplayMode(settings.errorDisplay, { preserveVisibility: true });
            applyLanguage(settings.language);
            if (isPanelVisible() && activeSection === 'history') {
                renderHistory();
            }
        });

        applyLanguage(settingsManager.get().language);
        showSection(activeSection);

        return root;
    }

    function createSelectRow({ label, value, options, onChange }) {
        const container = document.createElement('label');
        container.style.display = 'flex';
        container.style.flexDirection = 'column';
        container.style.gap = '4px';
        container.style.cursor = 'pointer';

        const span = document.createElement('span');
        span.textContent = label;
        span.style.fontWeight = '500';

        const select = document.createElement('select');
        select.style.padding = '6px';
        select.style.borderRadius = '4px';
        select.style.border = '1px solid #ccc';
        select.style.fontFamily = 'inherit';
        select.style.fontSize = '14px';

        options.forEach(({ value: optionValue, label: optionLabel }) => {
            const option = document.createElement('option');
            option.value = optionValue;
            option.textContent = optionLabel;
            select.appendChild(option);
        });

        select.value = value;

        select.addEventListener('change', (event) => {
            if (typeof onChange === 'function') {
                onChange(event.target.value);
            }
        });

        container.appendChild(span);
        container.appendChild(select);

        return { container, select, label: span };
    }

    function styleButton(button) {
        button.style.backgroundColor = '#007bff';
        button.style.color = '#fff';
        button.style.border = 'none';
        button.style.padding = '8px 12px';
        button.style.borderRadius = '4px';
        button.style.cursor = 'pointer';
        button.style.fontFamily = 'inherit';
        button.style.fontSize = '14px';
        button.style.boxShadow = '0 1px 3px rgba(0, 0, 0, 0.2)';

        button.addEventListener('mouseenter', () => {
            button.style.backgroundColor = '#0069d9';
        });

        button.addEventListener('mouseleave', () => {
            button.style.backgroundColor = '#007bff';
        });
    }

    function createToastContainer() {
        const toastContainerId = 'qrpc-toast-container';
        let container = document.getElementById(toastContainerId);
        if (!container) {
            container = document.createElement('div');
            container.id = toastContainerId;
            container.style.position = 'fixed';
            container.style.top = '16px';
            container.style.right = '16px';
            container.style.zIndex = '9999';
            container.style.display = 'flex';
            container.style.flexDirection = 'column';
            container.style.gap = '8px';
            document.body.appendChild(container);
        }
        return container;
    }

    function formatTimestamp(timestamp, locale) {
        try {
            return new Intl.DateTimeFormat(locale, {
                year: 'numeric',
                month: '2-digit',
                day: '2-digit',
                hour: '2-digit',
                minute: '2-digit',
                second: '2-digit'
            }).format(new Date(timestamp));
        } catch (_) {
            return new Date(timestamp).toLocaleString();
        }
    }

    function createCopyButton(text, language) {
        const button = document.createElement('button');
        button.type = 'button';
        button.textContent = language === 'ja' ? 'コピー' : 'Copy';
        button.style.border = '1px solid #007bff';
        button.style.backgroundColor = '#f0f6ff';
        button.style.color = '#007bff';
        button.style.padding = '4px 8px';
        button.style.borderRadius = '4px';
        button.style.cursor = 'pointer';
        button.style.fontFamily = 'inherit';
        button.style.fontSize = '12px';

        const successText = language === 'ja' ? 'コピーしました' : 'Copied!';
        const resetButton = () => {
            button.disabled = false;
            button.textContent = language === 'ja' ? 'コピー' : 'Copy';
        };

        const handleSuccess = () => {
            button.disabled = true;
            button.textContent = successText;
            setTimeout(() => {
                resetButton();
            }, 1500);
        };

        const fallbackCopy = () => {
            const textarea = document.createElement('textarea');
            textarea.value = text;
            textarea.setAttribute('readonly', '');
            textarea.style.position = 'fixed';
            textarea.style.opacity = '0';
            document.body.appendChild(textarea);
            textarea.select();
            try {
                document.execCommand('copy');
                handleSuccess();
            } catch (_) {
                resetButton();
            }
            document.body.removeChild(textarea);
        };

        button.addEventListener('click', (event) => {
            event.stopPropagation();
            if (!text) {
                return;
            }
            if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
                navigator.clipboard.writeText(text)
                    .then(() => {
                        handleSuccess();
                    })
                    .catch(() => {
                        fallbackCopy();
                    });
            } else {
                fallbackCopy();
            }
        });

        return button;
    }

    function getErrorDetailLabels(language) {
        if (language === 'ja') {
            return {
                error: 'エラー',
                message: 'メッセージ',
                id: 'id',
                faultInsights: 'エラーの種類'
            };
        }
        return {
            error: 'error',
            message: 'message',
            id: 'id',
            faultInsights: 'fault insights'
        };
    }

    function formatFaultSourceLabel(value, language) {
        if (value === null || typeof value === 'undefined') {
            return '';
        }
        const rawValue = typeof value === 'string' ? value : String(value);
        if (language !== 'ja') {
            return rawValue;
        }
        const normalized = rawValue.toLowerCase();
        const catalog = {
            client: 'クライアント',
            server: 'サーバー',
            external: '外部',
            unknown: '不明'
        };
        const label = Object.prototype.hasOwnProperty.call(catalog, normalized)
            ? catalog[normalized]
            : '不明';
        return `${label} (${rawValue})`;
    }

    function capitalizeFirstChar(text) {
        if (typeof text !== 'string' || text.length === 0) {
            return text;
        }
        return text.charAt(0).toUpperCase() + text.slice(1);
    }

    function formatFaultInsights(detail, language) {
        if (!detail || typeof detail !== 'object') {
            return '';
        }
        const hasSource = !(detail.fault_source === null || typeof detail.fault_source === 'undefined');
        const hasKnownFlag = typeof detail.is_known === 'boolean';
        const hasRetryableFlag = typeof detail.is_retryable === 'boolean';

        if (!hasSource && !hasKnownFlag && !hasRetryableFlag) {
            return '';
        }

        if (language === 'ja') {
            const sourceLabel = hasSource ? formatFaultSourceLabel(detail.fault_source, language) : '';
            const segments = [];
            if (sourceLabel) {
                segments.push(`${sourceLabel} 起因で`);
            }
            if (hasKnownFlag) {
                segments.push(detail.is_known ? '既知の' : '未知の');
            }
            if (hasRetryableFlag) {
                segments.push(detail.is_retryable ? '一時エラー' : '恒久エラー');
            } else {
                segments.push('エラー');
            }
            let text = segments.join('');
            if (text.endsWith('で')) {
                text = text.slice(0, -1);
            }
            return text;
        }

        const descriptor = [];
        if (hasKnownFlag) {
            descriptor.push(detail.is_known ? 'known' : 'unknown');
        }
        if (hasRetryableFlag) {
            descriptor.push(detail.is_retryable ? 'temporary' : 'permanent');
        }
        const descriptorText = descriptor.join(' ');
        const sourceLabel = hasSource ? formatFaultSourceLabel(detail.fault_source, language) : '';

        let englishOutput = '';
        if (sourceLabel) {
            if (descriptorText) {
                englishOutput = `${descriptorText} error caused by ${sourceLabel}`;
            } else {
                englishOutput = `error caused by ${sourceLabel}`;
            }
        } else if (descriptorText) {
            englishOutput = `${descriptorText} error`;
        } else {
            englishOutput = 'error';
        }

        return capitalizeFirstChar(englishOutput);
    }

    function normalizeArrayValue(value) {
        if (!Array.isArray(value)) {
            return null;
        }
        return value
            .map((segment) => {
                if (segment === null || typeof segment === 'undefined') {
                    return '';
                }
                return String(segment).trim();
            })
            .filter((segment) => segment.length > 0)
            .join('.');
    }

    function buildErrorDetailContent({ detail, language, fallbackText }) {
        const fallback = typeof fallbackText === 'string'
            ? fallbackText
            : (fallbackText === null || typeof fallbackText === 'undefined'
                ? ''
                : String(fallbackText));

        if (!detail || typeof detail !== 'object') {
            return {
                content: fallback,
                hasStructuredDetail: false
            };
        }

        const labels = getErrorDetailLabels(language);
        const container = document.createElement('div');
        container.style.display = 'flex';
        container.style.flexDirection = 'column';
        container.style.gap = '12px';

        const createRow = (labelText, valueText, options = {}) => {
            const row = document.createElement('div');
            row.style.display = 'flex';
            row.style.flexDirection = 'column';
            row.style.gap = '4px';

            const labelEl = document.createElement('div');
            labelEl.style.fontWeight = '600';
            labelEl.textContent = `${labelText}:`;

            const valueWrapper = document.createElement('div');
            valueWrapper.style.display = options.inline ? 'flex' : 'block';
            if (options.inline) {
                valueWrapper.style.alignItems = 'center';
                valueWrapper.style.gap = '8px';
            }

            const valueEl = document.createElement('div');
            valueEl.style.whiteSpace = 'pre-wrap';
            valueEl.style.wordBreak = 'break-word';
            const hasValue = !(valueText === null || typeof valueText === 'undefined' || (typeof valueText === 'string' && valueText === ''));
            valueEl.textContent = hasValue ? String(valueText) : '—';

            valueWrapper.appendChild(valueEl);
            if (options.append) {
                valueWrapper.appendChild(options.append);
            }

            row.appendChild(labelEl);
            row.appendChild(valueWrapper);
            return row;
        };

        const normalizedId = (() => {
            const arrayJoined = normalizeArrayValue(detail.id);
            if (arrayJoined) {
                return arrayJoined;
            }
            if (detail.id === null || typeof detail.id === 'undefined') {
                return '';
            }
            return String(detail.id);
        })();

        const normalizedMessage = (() => {
            if (language === 'ja') {
                return detail.message_ja || detail.message || fallback;
            }
            return detail.message || detail.message_ja || fallback;
        })();

        const formattedFaultInsights = formatFaultInsights(detail, language);

        container.appendChild(createRow(labels.error, normalizedId));
        container.appendChild(createRow(labels.message, normalizedMessage));

        const uuidRowAppend = detail.uuid
            ? createCopyButton(detail.uuid, language)
            : null;
        container.appendChild(createRow(labels.id, detail.uuid, {
            inline: Boolean(uuidRowAppend),
            append: uuidRowAppend
        }));

        container.appendChild(createRow(labels.faultInsights, formattedFaultInsights));

        return {
            content: container,
            hasStructuredDetail: true
        };
    }

    function createModalContainer() {
        const modalId = 'qrpc-error-modal';
        let overlay = document.getElementById(modalId);
        if (overlay) {
            return overlay;
        }

        overlay = document.createElement('div');
        overlay.id = modalId;
        overlay.style.position = 'fixed';
        overlay.style.top = '0';
        overlay.style.left = '0';
        overlay.style.width = '100%';
        overlay.style.height = '100%';
        overlay.style.display = 'none';
        overlay.style.alignItems = 'center';
        overlay.style.justifyContent = 'center';
        overlay.style.backgroundColor = 'rgba(0, 0, 0, 0.45)';
        overlay.style.zIndex = '10005';

        const modal = document.createElement('div');
        modal.style.backgroundColor = '#ffffff';
        modal.style.padding = '16px';
        modal.style.borderRadius = '6px';
        modal.style.boxShadow = '0 4px 12px rgba(0, 0, 0, 0.2)';
        modal.style.maxWidth = '520px';
        modal.style.width = '90%';
        modal.style.display = 'flex';
        modal.style.flexDirection = 'column';
        modal.style.gap = '12px';
        modal.style.fontFamily = 'sans-serif';
        modal.style.color = '#333';
        modal.style.maxHeight = '80vh';
        modal.style.overflowY = 'auto';

        const title = document.createElement('div');
        title.dataset.role = 'qrpc-error-title';
        title.textContent = 'Error';
        title.style.fontWeight = 'bold';
        modal.appendChild(title);

        const messageEl = document.createElement('div');
        messageEl.id = 'qrpc-error-modal-message';
        messageEl.style.whiteSpace = 'pre-wrap';
        messageEl.style.overflowWrap = 'anywhere';
        modal.appendChild(messageEl);

        const closeButton = document.createElement('button');
        closeButton.dataset.role = 'qrpc-error-close';
        closeButton.textContent = 'Close';
        styleButton(closeButton);
        closeButton.style.alignSelf = 'flex-end';
        closeButton.addEventListener('click', () => {
            overlay.style.display = 'none';
        });

        overlay.addEventListener('keydown', (event) => {
            if (event.key === 'Escape') {
                overlay.style.display = 'none';
            }
        });

        overlay.addEventListener('click', (event) => {
            if (event.target === overlay) {
                overlay.style.display = 'none';
            }
        });

        modal.appendChild(closeButton);
        overlay.appendChild(modal);
        document.body.appendChild(overlay);
        document.addEventListener('keydown', (event) => {
            if (event.key === 'Escape') {
                overlay.style.display = 'none';
            }
        });
        return overlay;
    }

    function showToast(container, message) {
        if (!container) {
            return;
        }
        const toast = document.createElement('div');
        toast.textContent = message;
        toast.style.backgroundColor = '#d9534f';
        toast.style.color = '#fff';
        toast.style.padding = '8px 12px';
        toast.style.borderRadius = '4px';
        toast.style.boxShadow = '0 2px 6px rgba(0, 0, 0, 0.2)';
        toast.style.fontFamily = 'sans-serif';
        toast.style.fontSize = '14px';
        toast.style.maxWidth = '240px';
        toast.style.wordBreak = 'break-word';

        container.appendChild(toast);

        setTimeout(() => {
            container.removeChild(toast);
        }, 3000);
    }

    function showModal(overlay, content, language = 'en', options = {}) {
        if (!overlay) {
            return;
        }
        const messageEl = overlay.querySelector('#qrpc-error-modal-message');
        if (messageEl) {
            messageEl.innerHTML = '';
            const isNode = (value) => value && typeof value === 'object' && typeof value.nodeType === 'number';
            if (isNode(content)) {
                messageEl.appendChild(content);
            } else if (Array.isArray(content)) {
                content.forEach((item) => {
                    if (isNode(item)) {
                        messageEl.appendChild(item);
                    } else {
                        const text = item === null || typeof item === 'undefined' ? '' : String(item);
                        const span = document.createElement('span');
                        span.textContent = text;
                        messageEl.appendChild(span);
                    }
                });
            } else {
                const text = content === null || typeof content === 'undefined'
                    ? ''
                    : String(content);
                messageEl.textContent = text;
            }
        }
        const normalizedLang = language === 'ja' ? 'ja' : 'en';
        const titleEl = overlay.querySelector('[data-role="qrpc-error-title"]');
        if (titleEl) {
            if (Object.prototype.hasOwnProperty.call(options, 'titleText')) {
                titleEl.textContent = options.titleText;
            } else {
                titleEl.textContent = normalizedLang === 'ja' ? 'エラー' : 'Error';
            }
        }
        const closeButton = overlay.querySelector('[data-role="qrpc-error-close"]');
        if (closeButton) {
            closeButton.textContent = normalizedLang === 'ja' ? '閉じる' : 'Close';
        }
        overlay.style.display = 'flex';
    }

    function hideModal(overlay) {
        if (!overlay) {
            return;
        }
        overlay.style.display = 'none';
    }

    function resolveErrorMessage(error, language = 'en') {
        const normalizedLang = language === 'ja' ? 'ja' : 'en';
        const metadataDetail = error?.metadata?.error_detail;

        if (normalizedLang === 'ja') {
            const jaMessage = metadataDetail?.message_ja;
            if (jaMessage) {
                return jaMessage;
            }
        }

        const enMessage = metadataDetail?.message;
        if (enMessage) {
            return enMessage;
        }

        return error?.message || 'An unexpected error occurred.';
    }

    function parseResponseBody(body) {
        if (typeof body !== 'string') {
            return { ok: true, value: body };
        }
        const trimmed = body.trim();
        if (!trimmed) {
            return {
                ok: false,
                error: new SyntaxError('Empty response body')
            };
        }
        try {
            return {
                ok: true,
                value: JSON.parse(body)
            };
        } catch (error) {
            return {
                ok: false,
                error
            };
        }
    }

    function createQrpcResponseError(result) {
        const metadataErrorDetail = result?.metadata?.error_detail;
        const responseErrorDetail = result?.error_detail;
        const detail = metadataErrorDetail ||
            responseErrorDetail ||
            buildDefaultErrorDetail({
                id: [CLIENT_ERROR_NAMESPACE, 'server_error', 'unknown'],
                fault_source: 'server'
            });
        const message = detail?.message ||
            metadataErrorDetail?.message ||
            responseErrorDetail?.message ||
            'Call failed.';
        const error = new Error(message);
        const metadata = Object.assign(
            {
                success: false
            },
            result?.metadata || {}
        );
        if (!metadata.error_detail && detail) {
            metadata.error_detail = detail;
        }
        error.metadata = metadata;
        return error;
    }

    function createNetworkTransportError(originalError) {
        const error = createClientSideQrpcError({
            idSegments: ['network', 'request_failed'],
            faultSource: 'external',
            message: 'Network request failed. Please check your connection.',
            messageJa: 'ネットワーク要求に失敗しました。接続状況を確認してください。',
            isRetryable: true
        });
        return error;
    }

    function createHttpStatusError(response) {
        const status = typeof response?.status === 'number' ? response.status : 'unknown';
        const statusText = response?.statusText || '';
        const message = statusText
            ? `Server responded with HTTP ${status} (${statusText}).`
            : `Server responded with HTTP ${status}.`;
        const messageJa = statusText
            ? `サーバーから HTTP ${status}（${statusText}）が返されました。`
            : `サーバーから HTTP ${status} が返されました。`;
        const error = createClientSideQrpcError({
            idSegments: ['http_status', String(status)],
            faultSource: 'external',
            message,
            messageJa
        });
        error.status = status;
        error.statusText = statusText;
        error.response = response;
        return error;
    }

    function createResponseParseError(parseError) {
        const error = createClientSideQrpcError({
            idSegments: ['response', 'parse_failed'],
            faultSource: 'external',
            message: 'Received an unreadable response from the server.',
            messageJa: 'サーバーからのレスポンスを解析できませんでした。',
        });
        return error;
    }

    function ensureQrpcError(error) {
        if (error?.metadata?.error_detail || error?.metadata?.success === false) {
            return error;
        }
        return createClientSideQrpcError({
            idSegments: ['unexpected'],
            faultSource: 'client',
            message: error?.message || 'An unexpected error occurred.',
            messageJa: error?.message_ja
        });
    }

    function createClientSideQrpcError(options = {}) {
        const {
            idSegments = ['error'],
            faultSource = 'client',
            message = 'Unknown error.',
            messageJa,
            isKnown = true,
            isRetryable = false,
            shouldAutoRetry = false
        } = options;

        const detail = buildDefaultErrorDetail({
            id: [CLIENT_ERROR_NAMESPACE].concat(idSegments),
            fault_source: faultSource,
            message,
            message_ja: messageJa || message,
            is_known: Boolean(isKnown),
            is_retryable: Boolean(isRetryable),
            should_auto_retry: Boolean(shouldAutoRetry)
        });

        const metadata = {
            success: false,
            error_detail: detail,
            server_name: null,
            type: 'response',
            mode: 'normal',
            protocol: 'http',
            request_id: detail.uuid,
            rpc_uuid: detail.uuid
        };

        const error = new Error(detail.message);
        error.metadata = metadata;
        return error;
    }

    function buildDefaultErrorDetail(overrides = {}) {
        const detail = Object.assign({
            uuid: null,
            id: [CLIENT_ERROR_NAMESPACE, 'generic'],
            fault_source: 'unknown',
            message: 'Unknown error.',
            message_ja: '不明なエラー。',
            is_known: false,
            is_retryable: false,
            should_auto_retry: false
        }, overrides);
        if (!Array.isArray(detail.id)) {
            detail.id = [CLIENT_ERROR_NAMESPACE, 'generic'];
        }
        return detail;
    }

    global.QrpcVuePlugin = QrpcVuePlugin;
})(typeof window !== 'undefined' ? window : globalThis);
