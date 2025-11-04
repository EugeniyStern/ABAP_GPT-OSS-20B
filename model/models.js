sap.ui.define([
    "sap/ui/model/json/JSONModel"
], function(JSONModel) {
    "use strict";

    return {
        createMessageModel: function() {
            var oModel = new JSONModel({
                messages: [
                    {
                        sender: "Пользователь 1",
                        text: "Привет! Это первое сообщение в чате.",
                        timestamp: "2024-01-15 10:30",
                        icon: "sap-icon://employee",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 2",
                        text: "Здравствуйте! Как дела?",
                        timestamp: "2024-01-15 10:32",
                        icon: "sap-icon://customer",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    },
                    {
                        sender: "Пользователь 1",
                        text: "Отлично, спасибо! А у вас?",
                        timestamp: "2024-01-15 10:35",
                        icon: "sap-icon://employee",
                        Actions: [
                            { Text: "Action 1", Key: "action1" },
                            { Text: "Action 2", Key: "action2" }
                        ]
                    }
                ]
            });
            return oModel;
        }
    };
});

